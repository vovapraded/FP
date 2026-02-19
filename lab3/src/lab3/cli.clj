(ns lab3.cli
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str]))

;; Data definitions
(def algorithms
  [{:key :linear
    :short "-l"
    :long "--linear"
    :desc "Use linear interpolation"}
   {:key :newton
    :short "-n"
    :long "--newton"
    :desc "Use Newton polynomial interpolation"}
   {:key :lagrange
    :short "-g"
    :long "--lagrange"
    :desc "Use Lagrange polynomial interpolation"}])

(def cli-options
  (vec
   (concat
    (for [{:keys [short long desc]} algorithms]
      [short long desc])

    [["-s" "--step STEP" "Step for output discretization"
      :default 1.0
      :parse-fn parse-double
      :validate [pos? "Step must be positive"]]
     ["-w" "--window-size SIZE" "Window size for polynomial interpolation"
      :default 4
      :parse-fn parse-long
      :validate [#(>= % 2) "Window size must be at least 2"]]
     ["-d" "--delimiter DELIM" "CSV delimiter (default: semicolon)"
      :default ";"]
     ["-h" "--help" "Show help"]])))

;; API
(defn- validate-options [options]
  (when-not (some options (map :key algorithms))
    "At least one interpolation algorithm must be specified"))

(defn- usage [options-summary]
  (str/join "\n"
            ["Interpolation program - Lab 3"
             ""
             "Usage: program [options]"
             ""
             "Options:"
             options-summary
             ""
             "Examples:"
             "  cat data.csv | program --linear --step 0.5"
             "  program -n --newton -w 5 --step 0.1 < input.txt"
             ""]))

(defn- error-msg [errors]
  (str "The following errors occurred while parsing command:\n\n"
       (str/join "\n" errors)))

(defn parse-cli-args [args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)
        validation-error (validate-options options)]
    (cond
      (:help options)    {:exit-message (usage summary) :ok? true}
      errors             {:exit-message (error-msg errors) :ok? false}
      validation-error   {:exit-message (str validation-error "\n\n" (usage summary)) :ok? false}
      :else              {:ok? true :options options})))

(defn get-selected-algorithms [options]
  (filter options (map :key algorithms)))
