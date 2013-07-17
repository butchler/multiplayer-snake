(ns snake
  (:require [clojure.browser.repl :as repl]
            [cljs.core.async :refer [chan timeout <! >! put! take! alts! close!]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]
                   [snake.macros :refer [debug]]))

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

(defn un-nil [x]
  (if (nil? x)
    false
    x))

(defn on [firebase event-type]
  (let [c (chan 10)]
    (.on firebase event-type #(put! c (un-nil (.val %))) #(close! c))
    c))

(defn once [firebase event-type]
  (let [c (chan)]
    (.once firebase event-type #(put! c (un-nil (.val %))) #(close! c))
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
                                               (put! c dir))))
    c))

(defn update-pos [pos dir]
  (let [s 3
        dirs {"up"    [0 (- s)]
              "down"  [0 s]
              "left"  [(- s) 0]
              "right" [s 0]}]
    (mapv + pos (dirs dir))))

;; Main logic.
(defn start-game [game-ref us them]
  (go
    (let [tick (tick-chan (/ 1000 FPS))
          our-dir-ref (.child game-ref us)
          dir-chan (direction-chan)
          initial-dir (if (= us "player-1") "right" "left")
          their-dir-chan (on (.child game-ref them) "value")]

      (<! (timeout 1000))

      (loop [our-pos [50 50] their-pos [200 200] our-dir initial-dir]
        (.set our-dir-ref our-dir)
        (render our-pos their-pos)
        (<! tick)
        (alt!
          ;tick (recur our-pos their-pos our-dir)
          ;tick (recur (update-pos our-pos our-dir) (update-pos their-pos our-dir) our-dir)
          dir-chan ([new-dir]
                    (recur our-pos their-pos new-dir))
          their-dir-chan ([their-dir]
                          (recur (update-pos our-pos our-dir) (update-pos their-pos their-dir) our-dir)))))))

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

(debug (+ 1 1))
