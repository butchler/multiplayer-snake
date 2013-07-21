(defproject snake "0.1.0"
  :description "Multiplayer snake"
  :url "http://butchler.github.com/multiplayer-snake"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "LATEST"]]
  :plugins [[lein-cljsbuild "LATEST"]]
  :cljsbuild {:builds
              {:dev {:source-paths ["src" "core.async/src/main/clojure"]
                     :compiler {:output-to "snake.js"}}}})
