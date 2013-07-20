(ns snake
  (:require [clojure.browser.repl :as repl]
            [cljs.core.async :refer [chan timeout <! >! put! take! alts! close!]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(repl/connect "http://localhost:9000/repl")

;; Constants
(def fps 2)
(def cols 40)
(def rows 40)

;; Rendering
(defn el
  "Utility function for creating DOM elements."
  ([tag] (el tag {} ()))

  ([tag arg]
   (cond
     (map? arg) (el tag arg ())
     :else (el tag {} arg)))

  ([tag attributes children]
   (let [element (js/document.createElement tag)]

     ; Assign attributes
     (doseq [[attr value] attributes]
       (aset element (name attr) value))

     ; Append children.
     (doseq [child children]
       (.appendChild element child))

     element)))

(defn make-grid! []
  "Make a grid of cells (as <b> elements), append the grid to body, and return
  the cells as a matrix."
  (let [cells (vec (for [_ (range rows)]
                (vec (for [_ (range cols)]
                  (el "b")))))
        grid (el "div" {:className "grid"} (for [row cells]
                                             (el "div" {:className "row"} row)))]
    (js/document.body.appendChild grid)
    cells))

(def grid (make-grid!))

(defn set-cell! [[x y] class]
  (when-let [cell (get-in grid [y x])]
    (aset cell "className" class)))

(defn render-player! [state player]
  (set-cell! (first (get-in state [player :pos])) player))

(defn render! [state]
  (render-player! state "player-1")
  (render-player! state "player-2"))

;; Firebase
(def root (new js/Firebase "https://snake.firebaseio.com/"))

(defn path [name] (.child root name))

(defn nil->false [x]
  "nil cannot be sent through channels, so this is used to turn nil into false."
  (if (nil? x)
    false
    x))

(defn on [firebase event-type]
  "Creates a channel for a Firebase event."
  (let [c (chan 10)]
    (.on firebase event-type #(put! c (nil->false (.val %))) #(close! c))
    c))

(defn once [firebase event-type]
  "Creates a channel for a Firebase event that only executes once."
  (let [c (chan)]
    (.once firebase event-type #(put! c (nil->false (.val %))) #(close! c))
    c))

;; Utility
(defn tick-chan [ms]
  "Creates a channel that gets \"tick\" put onto it every ms milliseconds."
  (let [c (chan)]
    (js/setInterval #(put! c "tick") ms)
    c))

(defn direction-chan []
  "Creates a channel that gets direction strings (e.g. \"left\", \"up\") put
  onto it whenever the user presses a key."
  (let [c (chan)]
    (.addEventListener js/window "keydown" (fn [event]
                                             (when-let [dir (case (.-keyCode event)
                                                              37 "left"
                                                              38 "up"
                                                              39 "right"
                                                              40 "down"
                                                              nil)]
                                               ; Disable scrolling when user presses arrow keys.
                                               (.preventDefault event)
                                               (put! c dir))))
    c))

(defn move [dir pos]
  "Moves the given coordinates one cell in the given direction."
  (mapv + pos (case dir
                "up"    [ 0 -1 ]
                "down"  [ 0  1 ]
                "left"  [-1  0 ]
                "right" [ 1  0 ]
                [0 0])))

;; Game logic
(defn move-player [state player]
  (let [{:keys [dir pos]} (state player)
        new-positions (map (partial move dir) pos)]
    (assoc-in state [player :pos] new-positions)))

(defn step-game [state]
  (-> state
      (move-player "player-1")
      (move-player "player-2")))

;; Main loop
(def command-chan (chan))
(def step-chan (chan))
(set! js/window.kill #(put! command-chan "die"))
(set! js/window.log-state #(put! command-chan "log"))
(set! js/window.step #(put! step-chan "step"))

(defn start-game [game-ref us them]
  (let [tick (tick-chan (/ 1000 fps))
        our-data (.child game-ref us)
        their-data (on (.child game-ref them) "value")
        our-dir (direction-chan)
        initial-state {"player-1" {:pos (list [10 10]) :len 1 :dir "right"}
                       "player-2" {:pos (list [30 30]) :len 1 :dir "left"}
                       :frame 0}]

    (when (= us "player-1")
      (.set our-data (clj->js {:dir (get-in initial-state [us :dir])
                               :frame (:frame initial-state)})))

    (go
      (loop [state initial-state next-dir (get-in initial-state [us :dir])]
        (alt!
          our-dir ([new-dir]
                   (recur state new-dir))
          command-chan ([command]
                        (when (= command "log")
                          (console/log (str state))
                          (recur state next-dir)))
          their-data ([data]
                      (if data
                        (let [_ (.set our-data (clj->js {:dir (if (= us "player-1") (get-in state [us :dir]) next-dir)
                                                         :frame (inc (:frame state))}))
                              their-dir (aget data "dir")
                              new-state (-> state
                                            (assoc-in [them :dir] their-dir)
                                            step-game
                                            (assoc-in [us :dir] next-dir)
                                            (update-in [:frame] inc))]

                          (console/log (str new-state))
                          (render! new-state)
                          #_ (let [state (if (= us "player-2") state new-state)]
                               (console/log (str new-state))
                               (render! new-state))

                          (<! tick)

                          (recur new-state next-dir))
                        (recur state next-dir)))
          :priority true)))

    (close! their-data)))

;; Joining
(defn join-game []
  (go
    (let [game-to-join (path "/game-to-join")]
      (if-let [game-url (<! (once game-to-join "value"))]
        ;; If there is an existing game, join it and clear game-to-join.
        (let [game-ref (new js/Firebase game-url)]
          (.remove game-to-join)
          (start-game game-ref "player-2" "player-1"))
        ;; Otherwise, make a new game and put it's URL in game-to-join.
        (let [_ (.remove root)
              game-ref (.push (path "/games"))
              game-url (.toString game-ref)]
          (.set game-to-join game-url)
          (start-game game-ref "player-1" "player-2"))))))

(join-game)
