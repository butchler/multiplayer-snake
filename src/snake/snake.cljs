(ns snake
  (:require [clojure.browser.repl :as repl]
            [cljs.core.async :refer [chan timeout <! >! put! take! alts! close!]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(repl/connect "http://localhost:9000/repl")

;; Constants
(def fps 5)
(def cols 40)
(def rows 40)
(def initial-length 5)
(def initial-state {"player-1" {:positions (repeat initial-length [10 10])
                                :direction "right"}
                    "player-2" {:positions (repeat initial-length [30 30])
                                :direction "left"}})

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

(defn make-grid!
  "Make a grid of cells (as <b> elements), append the grid to body, and return
  the cells as a matrix."
  []
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

(defn render-player! [prev-state state player]
  (let [positions (get-in state [player :positions])
        prev-positions (get-in prev-state [player :positions])]
    (set-cell! (last prev-positions) "")
    (set-cell! (first positions) player)))

(defn render! [prev-state state]
  (render-player! prev-state state "player-1")
  (render-player! prev-state state "player-2"))

;; Firebase
(def root (new js/Firebase "https://snake.firebaseio.com/"))

(defn path [name] (.child root name))

(defn nil->false
  "nil cannot be sent through channels, so this is used to turn nil into false."
  [x]
  (if (nil? x)
    false
    x))

(defn on
  "Creates a channel for a Firebase event."
  ([firebase-ref event-type] (on firebase-ref event-type identity))
  ([firebase-ref event-type wrapper-fn]
   (let [c (chan 10)]
     (.on firebase-ref event-type #(put! c (wrapper-fn (.val %))) #(close! c))
     c)))

(defn once
  "Creates a channel for a Firebase event that only executes once."
  ([firebase-ref event-type] (once firebase-ref event-type identity))
  ([firebase-ref event-type wrapper-fn]
   (let [c (chan)]
     (.once firebase-ref event-type #(put! c (wrapper-fn (.val %))) #(close! c))
     c)))

;; Utility
(defn tick-chan
  "Creates a channel that gets \"tick\" put onto it every ms milliseconds."
  [ms]
  (let [c (chan)]
    (js/setInterval #(put! c "tick") ms)
    c))

(defn arrow-chan
  "Creates a channel that gets direction strings (e.g. \"left\", \"up\") put
  onto it whenever the user presses an arrow key."
  []
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

(defn move
  "Moves the given coordinates one cell in the given direction."
  [pos dir]
  (mapv + pos (case dir
                "up"    [ 0 -1 ]
                "down"  [ 0  1 ]
                "left"  [-1  0 ]
                "right" [ 1  0 ]
                [0 0])))

(defn get-direction [from to]
  (let [difference (mapv - to from)]
    (case difference
      [ 0 -1 ] "up"
      [ 0  1 ] "down"
      [-1  0 ] "left"
      [ 1  0 ] "right"
      nil)))

;; Game logic
(defn is-dead? [state player]
  (let [{:keys [direction positions]} (state player)
        head (first positions)
        next-head (move head direction)
        opponent (if (= player "player-1") "player-2" "player-1")
        opponent-positions (get-in state [opponent :positions])]
    (if (some #(= next-head %) (concat positions opponent-positions))
      true
      (let [[x y] next-head]
        (or (< x 0)
            (>= x cols)
            (< y 0)
            (>= y rows))))))

(defn move-player [state player]
  (let [dead? (or (:dead? (state player)) (is-dead? state player))]
    (if dead?
      (assoc-in state [player :dead?] dead?)
      (let [{:keys [direction positions]} (state player)
            directions (cons direction (map get-direction (drop 1 positions) positions))
            new-positions (map move positions directions)]
        (-> state
            (assoc-in [player :positions] new-positions)
            (assoc-in [player :dead?] dead?))))))

;; Main loop
(def commands (chan))
(def steps (chan))
(set! js/window.kill #(put! commands "die"))
(set! js/window.log-state #(put! commands "log"))
(set! js/window.step #(put! steps "step"))

(defn start-game [game-ref us them]
  (let [ticks (tick-chan (/ 1000 fps))
        arrow-keys (arrow-chan)
        their-updates (on (.child game-ref them) "value")
        send-update #(.set (.child game-ref us) (clj->js {:direction (get-in % [us :direction])
                                                          :frame (:frame %)}))
        initial-state (assoc initial-state :frame 0)]
    (go
      (loop [state (if (= us "player-1")
                     (do
                       ;; The first player does the first update to get things rolling.
                       (send-update initial-state)
                       (move-player initial-state us))
                     initial-state)]
        (alt!
          arrow-keys ([new-direction]
                      (recur (assoc-in state [us :direction] new-direction)))
          commands ([command]
                    (when (= command "log")
                      (console/log (str state))
                      (recur state)))
          their-updates ([data]
                         ;; When the other player updates, we update.
                         (let [their-direction (aget data "direction")
                               new-state (-> state
                                             (assoc-in [them :direction] their-direction)
                                             (move-player "player-1")
                                             (move-player "player-2")
                                             (update-in [:frame] inc))]
                           (send-update new-state)
                           (render! state new-state)
                           (<! ticks)
                           #_(when (= us "player-1")
                               (<! steps))
                           (recur new-state)))
          :priority true))

      ;; Is it necessary to close all of the channels? Are they closed when
      ;; they are garbage-collected?
      (close! commands)
      (close! steps)
      (close! ticks)
      (close! arrow-keys)
      (close! their-updates))))

;; Joining
(defn join-game []
  (go
    (let [game-to-join (path "/game-to-join")]
      (if-let [game-url (<! (once game-to-join "value" nil->false))]
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
