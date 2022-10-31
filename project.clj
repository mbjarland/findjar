(defproject mbjarland/findjar "1.0.4"
  :description "findjar utility - capable of searching within zip/jar archives"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.cli "1.0.214"]
                 ;[pandect/pandect "1.0.2"]
                 [jansi-clj "1.0.1"]
                 [com.taoensso/tufte "2.3.0"]]
  :main findjar.main
  :aot [findjar.main]
  :bin {:name                   "findjar"
        :bootclasspath          true
        :custom-preamble-script "preamble.sh"}
  :plugins [[lein-binplus "0.6.6"]
            [me.arrdem/lein-git-version "2.0.8"]]
  ;:clean-targets ^{:protect false} [:target-path "gen-resources"]
  :git-version {:version-file      "gen-resources/build/version.edn"
                :version-file-keys [:ref :version :timestamp :dirty? :ref-short]}
  :resource-paths ["resources" "gen-resources"]

  )
