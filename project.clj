(defproject mbjarland/findjar "1.0.2"
  :description "findjar utility - capable of searching within zip/jar archives"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.cli "0.4.1"]
                 [pandect/pandect "0.6.1"]
                 [jansi-clj "0.1.1"]
                 [com.taoensso/tufte "2.0.1"]]
  :main findjar.main
  :aot [findjar.main]
  :bin {:name                   "findjar"
        :bootclasspath          true
        :custom-preamble-script "preamble.sh"}
  :plugins [[lein-binplus "0.6.4"]
            [me.arrdem/lein-git-version "2.0.8"]]
  :clean-targets ^{:protect false} [:target-path "gen-resources"]
  :git-version {:version-file      "gen-resources/build/version.edn"
                :version-file-keys [:ref :version :timestamp :dirty? :ref-short]}
  :resource-paths ["resources" "gen-resources"]

  )
