(defproject mbjarland/findjar "1.0.1"
  :description "findjar utility - capable of searching within zip/jar archives"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [pandect/pandect "0.6.1"]]
  :main findjar.core
  :aot [findjar.core]
  :bin { :name "findjar" 
         :bootclasspath true
         :custom-preamble-script "preamble.sh" }
  :plugins [[lein-binplus "0.6.4"]])
