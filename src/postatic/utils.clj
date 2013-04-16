(ns postatic.utils
  (:require [clojure.java.io :as io]))

(def verbose-log true)

(defmacro log [& xs]
  `(when verbose-log (println ~@xs)))

(defn ends-with [suffix s]
  (if (.endsWith s suffix) s (str s suffix)))

(defn dircat [dir filename]
  (str (ends-with "/" (str dir)) (str filename)))

(defn remove-chars [chars s]
  (let [cset (set chars)]
    (->> s
         seq
         (filter #(not (cset %)))
         (apply str))))

(defn load-props
  [filename]
  (with-open [^java.io.Reader reader (io/reader filename)] 
    (let [props (java.util.Properties.)]
      (.load props reader)
      (into {} (for [[k v] props] [(keyword k) v])))))
