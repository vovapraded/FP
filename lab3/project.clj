(defproject lab3 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.cli "1.0.219"]]
  :plugins [[lein-cljfmt "0.9.2"]
            [lein-kibit "0.1.8"]
            [lein-ancient "1.0.0-RC3"]
            [lein-bikeshed "0.5.2"]
            [com.github.clj-kondo/lein-clj-kondo "2024.03.13"]]
  :main lab3.core
  :aot [lab3.core]
  :repl-options {:init-ns lab3.core}

  :kibit {:replace true}

  :aliases {"lint"     ["do"
                        ["clj-kondo" "--lint" "src" "test"]
                        ["cljfmt" "check"]
                        ["kibit"]
                        ["bikeshed" "--max-line-length" "100"]]
            "lint-fix" ["cljfmt" "fix"]})
