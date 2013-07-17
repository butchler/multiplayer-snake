(ns snake
  (:require [clojure.browser.repl :as repl]
            [cljs.core.async :refer [chan timeout <! >! put! take! alts! close!]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(repl/connect "http://localhost:9000/repl")

;; Constants
(def FPS 10)
(def width 400)
(def height 400)
(def radius 10)

;; Init canvas.
(def canvas (js/document.createElement "canvas"))
(js/document.body.appendChild canvas)
(set! (.-width canvas) width)
(set! (.-height canvas) height)
(def brush (.getContext canvas "2d"))

(defn fill-circle [radius x y]
  (doto brush
    (.beginPath)
    (.arc x y radius 0 (* 2 js/Math.PI) false)
    (.closePath)
    (.fill)))

(defn render [our-pos their-pos]
  (.clearRect brush 0 0 width height)
  (set! (.-fillStyle brush) "blue")
  (apply fill-circle radius our-pos)
  (set! (.-fillStyle brush) "red")
  (apply fill-circle radius their-pos))

;; Init Firebase.
(def root (new js/Firebase "https://snake.firebaseio.com/"))

(defn path [name] (.child root name))

(defn nil->false [x]
  ;; nil cannot be sent through channels, so this is used to turn nil into false.
  (if (nil? x)
    false
    x))

(defn on [firebase event-type]
  (let [c (chan 10)]
    (.on firebase event-type #(put! c (nil->false (.val %))) #(close! c))
    c))

(defn once [firebase event-type]
  (let [c (chan)]
    (.once firebase event-type #(put! c (nil->false (.val %))) #(close! c))
    c))

;; Utility functions.
(defn tick-chan [ms]
  (let [c (chan)]
    (js/setInterval #(put! c "tick") ms)
    c))

(defn direction-chan []
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

(defn update-pos [pos dir]
  (let [offsets {"up"    [ 0 -1 ]
                 "down"  [ 0  1 ]
                 "left"  [-1  0 ]
                 "right" [ 1  0 ]}]
    (mapv + pos (offsets dir))))

;; Main logic.
(defn start-game [game-ref us them]
  (go
    (let [tick (tick-chan (/ 1000 FPS))
          our-ref (.child game-ref us)
          dir-chan (direction-chan)
          their-chan (on (.child game-ref them) "value")
          initial-state {"player-1" {:pos [50 50] :their-pos [200 200] :dir "right" :frame 0}
                         "player-2" {:pos [200 200] :their-pos [50 50] :dir "left" :frame 0}}]

      (<! (timeout 1000))

      (loop [state (initial-state us)]
        (set! js/window.s state)
        (let [{:keys [dir pos their-pos frame]} state]
          (.set (.child our-ref "dir") dir)
          (.set (.child our-ref "frame") frame)
          (render pos their-pos)
          (<! tick)
          (recur
            (->
              (alt!
                dir-chan ([new-dir]
                          (assoc state :dir new-dir))
                their-chan ([their-data]
                            (if-let [their-dir (aget their-data "dir")]
                              (-> state
                                  (assoc :pos (update-pos pos dir))
                                  (assoc :their-pos (update-pos their-pos their-dir)))
                              state))
                :priority true)
              (assoc :frame (inc frame)))))))))

;; Try to join game.
(go
  (let [waiting (path "/waiting")]
    (if-let [game-url (<! (once waiting "value"))]
      ;; If there is an existing game, join it and clear waiting.
      (let [game-ref (new js/Firebase game-url)]
        (.remove waiting)
        (start-game game-ref "player-2" "player-1"))
      ;; Otherwise, make a new game and put it's URL in waiting.
      (let [game-ref (.push (path "/games"))
            game-url (.toString game-ref)]
        (.set waiting game-url)
        (start-game game-ref "player-1" "player-2")))))

