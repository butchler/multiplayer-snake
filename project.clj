(defproject snake "0.1.0"
  :description "Multiplayer snake"
  :url "http://butchler.github.com/multiplayer-snake"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.0-SNAPSHOT"]]
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :plugins [[lein-cljsbuild "LATEST"]]
  :cljsbuild {:builds
              {:dev {:source-paths ["src"]
                     :compiler {:output-to "snake.js"}}}})
