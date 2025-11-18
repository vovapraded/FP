(defproject lab1 "0.1.0-SNAPSHOT"
  :description "Functional Programming Lab 1 - Project Euler solutions"
  :url "https://github.com/your-username/fp-lab1"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/test.check "1.1.1"]]
  :plugins [[lein-cljfmt "0.9.2"]
            [lein-kibit "0.1.8"]
            [lein-ancient "1.0.0-RC3"]
            [lein-bikeshed "0.5.2"]
            [com.github.clj-kondo/lein-clj-kondo "2024.03.13"]]  ; ДОБАВЛЕНО
  :profiles {:dev {:dependencies [[criterium "0.4.6"]
                                  [org.clojure/tools.namespace "1.4.4"]]}}
  :test-paths ["test"]
  :java-source-paths ["src"]
  :javac-options ["-target" "17" "-source" "17"]
  :repl-options {:init-ns lab1.core}

  :kibit {:replace true}

  :aliases {"lint" ["do"
                    ["clj-kondo" "--lint" "src" "test"]
                    ["cljfmt" "check"]
                    ["kibit"]
                    ["bikeshed" "--max-line-length" "100"]]
            "lint-fix" ["cljfmt" "fix"]
            "check-deps" ["ancient"]
            "test-coverage" ["test"]})
