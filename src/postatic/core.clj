(ns postatic.core
  (:gen-class)
  (:require [net.cgrand.enlive-html :as enl]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clj-time.format :as dt])
  (:use [postatic.utils]
        [clojure.tools.cli :only [cli]]
        [clj-time.core :only [after? date-time now year]]))

;; Article stuff

(def ddmmyyyy (dt/formatter "dd.MM.yyyy"))

(defn string-of-content
  [nodes selector]
  (apply str (-> nodes (enl/select selector) first :content)))

(defn local-ref?
  [url]
  (not (.contains url "://")))

(defn article-file?
  [file]
  (-> file .getName (.startsWith "a__")))

(defn local-resources
  [nodes]
  (let [srcs (-> nodes
                 (enl/select [#{:img :source}])
                 (->> (map #(-> % :attrs :src))))
        hrefs (-> nodes
                  (enl/select [[:a (enl/attr? :href)]])
                  (->> (map #(-> % :attrs :href))))]
    (->> (concat srcs hrefs)
         (filter local-ref?))))

(defn read-article
  [file]
  (let [html (enl/html-resource file)
        topics (-> html (string-of-content [:head :topics]) (string/split #"\s"))
        date (-> html (string-of-content [:head :date]))
        title (-> html (string-of-content [:h1]))
        refs (-> html local-resources)]
    (log "Found:" title date)
    {:title title
     :date (dt/parse ddmmyyyy date)
     :topics topics
     :file file
     :filerefs refs
     :content (-> html (enl/select [:#page]) first :content)}))

(defn article-href
  [article]
  (-> article :file .getName))

(defn enrich-article
  [article]
  (let [date (dt/unparse ddmmyyyy (:date article))
        title (:title article)
        href (article-href article)]
    (assoc article :content
           (enl/at (:content article)
                   [:h1] (enl/substitute (enl/html
                                          [:a {:name href}]
                                          [:h1 title]
                                          date " " [:a {:href href} "Permlink"] [:p]))))))

(defn read-articles
  [dir]
  (log "Reading articles from" dir)
  (->> (file-seq dir)
       (filter article-file?)
       (map read-article)
       (map enrich-article)
       (sort #(.compareTo (:date %2) (:date %1)))))

(defn read-me
  [dir]
  (enl/html-resource (io/file (dircat dir "me.html"))))

;; Grouping and Queries

(defn from-current-year
  [articles]
  (let [jan-1st (date-time (year (now)) 1 1)]
    (take-while #(after? (:date %) jan-1st) articles)))

(defn group-by-year
  [articles]
  (group-by #(-> % :date year str) articles))

(defn group-by-topic
  [articles]
  (->> articles
       (mapcat (fn [x]
                 (map #(vector % x)
                      (:topics x))))
       (group-by first)
       (map (fn [[k v]]
              [k (vec (map second v))]))
       (into {})))

;; Production Helpers

(defn emit-html
  [nodes]
  (apply str nodes))

(defn produce-file
  [target-file template-fn content]
  (log "Producing" target-file)
  (spit target-file (emit-html (template-fn content))))

(defn copy-common-resources
  [resources-dir output-dir]
  (log "Copying common resources from" resources-dir)
  (doseq [src (file-seq resources-dir)]
    (if (.isFile src)
      (let [filename (.getName src)
            dst (io/file (dircat output-dir filename))]
        (io/copy src dst)))))

(defn copy-article-resources
  [articles-dir output-dir articles]
  (log "Copying local files referenced by articles from" articles-dir)
  (doseq [a articles, fileref (:filerefs a)
          :let [src (io/file (dircat articles-dir fileref))
                dst (io/file (dircat output-dir fileref))]
          :when (not (article-file? src))]
    (io/copy src dst)))

(defn clean-dir
  [dir]
  (log "Cleaning" dir)
  (doseq [file (.listFiles dir)]
    (io/delete-file file true))
  (.mkdir dir))

(defn template-html
  [templates-dir]
  (enl/html-resource (io/file (dircat templates-dir "main.html"))))

;; Html Production using templates

(defn article-link
  [article]
  [:tr
   [:td (dt/unparse ddmmyyyy (:date article))]
   [:td [:a {:href (article-href article)} (:title article)]]])

(defn list-groups
  [groups]
  (map (fn [[title articles]]
         (enl/html [:h1 (if (string/blank? title) "Misc" title)]
                   [:table (map article-link articles)]))
       groups))

(defn produce-index
  [templates-dir output-dir articles]
  (let [template-fn (enl/template (template-html templates-dir) [articles]
                                  [:#content] (apply enl/content (->> articles
                                                                      from-current-year
                                                                      (map :content)
                                                                      (interpose (enl/html [:p] [:hr])))))]
    (produce-file (dircat output-dir "index.html") template-fn articles)))

(defn produce-articles
  [templates-dir output-dir articles]
  (let [template-fn (enl/template (template-html templates-dir) [a]
                                  [:#content] (apply enl/content (:content a)))]
    (doseq [a articles]
      (produce-file (dircat output-dir (-> a :file .getName)) template-fn a))))

(defn produce-by-year
  [templates-dir output-dir articles]
  (let [template-fn (enl/template (template-html templates-dir) [articles]
                                  [:#content] (apply enl/content (->> articles
                                                                      group-by-year
                                                                      list-groups)))]
    (produce-file (dircat output-dir "by-year.html") template-fn articles)))

(defn produce-by-topic
  [templates-dir output-dir articles]
  (let [template-fn (enl/template (template-html templates-dir) [articles]
                                  [:#content] (apply enl/content (->> articles
                                                                      group-by-topic
                                                                      list-groups)))]
    (produce-file (dircat output-dir "by-topic.html") template-fn articles)))

(defn produce-about
  [templates-dir output-dir me]
  (let [template-fn (enl/template (template-html templates-dir) [_]
                                  [:#content] (enl/content me))]
    (produce-file (dircat output-dir "about.html") template-fn nil)))


;; Main production process

(defn produce
  [input-dir output-dir]
  (let [templates-dir (io/file (dircat input-dir "templates"))
        articles-dir (io/file (dircat input-dir "articles"))
        resources-dir (io/file (dircat input-dir "resources"))]
    (log "All outputs are written to" output-dir)
    (clean-dir output-dir)
    (let [articles (read-articles articles-dir)]
      (produce-articles templates-dir output-dir articles)
      (produce-index templates-dir output-dir articles)
      (produce-by-year templates-dir output-dir articles)
      (produce-by-topic templates-dir output-dir articles)
      (copy-article-resources articles-dir output-dir articles))
    (let [me (read-me articles-dir)]
      (produce-about templates-dir output-dir me))
    (copy-common-resources resources-dir output-dir)
    (log "Done.")))

(defn produce-sample []
  (produce (io/file "./sample-data/input") (io/file "./sample-data/output")))


(defn -main [& args]
  (let [[params commands usage]
        (cli args
             ["-v" "--verbose" "Write log to stdout." :flag true :default false]
             ["-c" "--config" "Configuration properties file." :default "postatic.properties"])]
    (alter-var-root #'verbose-log (fn [_] (:verbose params)))
    (let [cfg (load-props (:config params))
          input-dir (-> cfg :input.dir io/file)
          output-dir (-> cfg :output.dir io/file)]
      (doseq [c commands]
        (case c
          "produce" (produce input-dir output-dir)
          "clean" (clean-dir output-dir))))))