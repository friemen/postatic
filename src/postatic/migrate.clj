(ns postatic.migrate
  (:require [net.cgrand.enlive-html :as enl]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clj-time.format :as dt])
  (:use [postatic.utils]))


(defn anker? [node]
  (and (= :a (:tag node))
       (not (nil? (-> node :attrs :name)))))

(defn ddmmyyyy-to-yyyymmdd
  [date]
  (string/join "-" (-> date (string/split #"\u002e") reverse)))

(defn article-filename
  [date title]
  (str "a__"
       (ddmmyyyy-to-yyyymmdd date)
       "__"
       (remove-chars "/?.,'\"%" title)
       ".html"))

(defn parse-article
  [nodes]
  (let [title-parts (-> nodes
                        (enl/select [:h4])
                        first
                        :content
                        first
                        (string/split #", "))
        title (->> title-parts drop-last (string/join ", "))
        date (last title-parts)
        imgrefs (-> nodes
                 (enl/select [#{:img :source}])
                 (->> (map #(-> % :attrs :src))))]
    {:title title
     :date date
     :file (io/file (article-filename date title))
     :filerefs imgrefs
     :content nodes}))

(defn article-resource-filename
  [a bare-filename]
  (str (-> a :date ddmmyyyy-to-yyyymmdd) "__" bare-filename))

(defn read-articles
  [src-dir]
  (let [src-file (-> src-dir
                     (dircat (str (last
                                   (string/split src-dir #"/"))
                                  ".html"))
                     io/file)
        article-content (-> src-file
                            enl/html-resource
                            (enl/select [:tr])
                            (nth 2)
                            (enl/select [:td])
                            (nth 2)
                            :content)]
    (log src-file)
    (->> article-content
                   (partition-by anker?)
                   (drop 1)
                   (partition 2)
                   (map (partial apply concat))
                   (map parse-article))))

(def t (java.io.StringReader. "
<html>
  <head>
    <link href='styles.css' type='text/css' rel='stylesheet' />
    <link href='pygments.css' type='text/css' rel='stylesheet' />
    <meta charset='utf-8' />
  </head>
  <body>
    <div id='page'/>
  </body>
</html>"))


(enl/deftemplate article t [a]
  [:#page] (apply enl/content (:content a))
  [:head :meta] (enl/after (enl/html [:date (:date a)]))
  [:h4] (enl/substitute (enl/html [:h1 (:title a)]))
  [:hr] (enl/substitute nil)
  [#{:img :source}] #(assoc-in % [:attrs :src]
                               (article-resource-filename a (-> % :attrs :src))))

(defn migrate-articles
  [src-dir dst-dir]
  (doseq [a (read-articles src-dir)]
    (let [dst-file (io/file (dircat dst-dir (:file a)))]
      (log dst-file)
      (spit dst-file (apply str (article a)))
      (doseq [fileref (:filerefs a)]
        (io/copy (io/file (dircat src-dir fileref))
                 (io/file (dircat dst-dir (article-resource-filename a fileref))))))))


(doseq [year (range 2006 2014)]
  (migrate-articles (dircat "/home/riemensc/Web/blog" year)
                    "/home/riemensc/Web/migration"))