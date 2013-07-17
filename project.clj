(defproject cljs-async "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "LATEST"]]
  :plugins [[lein-cljsbuild "LATEST"]]
  ;:source-paths ["src" "core.async/src/main/clojure"]
  :cljsbuild {:builds
              ;{:dev {:source-paths ["src" "core.async/src/main/clojure/cljs"]
              {:dev {:source-paths ["src" "core.async/src/main/clojure"]
                     :compiler {:output-to "snake.js"}}}})
