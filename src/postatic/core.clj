(ns postatic.core
  (:gen-class)
  (:require [net.cgrand.enlive-html :as enl]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.xml :as xml]
            [clj-time.format :as dt])
  (:use [postatic.utils]
        [clojure.tools.cli :only [cli]]
        [clj-time.core :only [after? date-time millis now year]]))

;; Article stuff

(def ddmmyyyy (dt/formatter "dd.MM.yyyy"))
(def rfc3339 (dt/formatter "yyyy-MM-dd'T'HH:mm:ss.SSSZZ"))

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
  (-> article :file .getName (string/escape {\space "%20"})))

(defn enrich-article
  [cfg article]
  (let [date (dt/unparse ddmmyyyy (:date article))
        title (:title article)
        href (article-href article)]
    (assoc article
      :content (enl/at (:content article)
                       [:h1] (enl/substitute (enl/html
                                              [:a {:name href}]
                                              [:h1 title]
                                              date " " [:a {:href href} "Permalink"] [:p])))
      :author (:author cfg)
      :email (:email cfg))))

(defn read-articles
  [cfg dir]
  (log "Reading articles from" dir)
  (->> (file-seq dir)
       (filter article-file?)
       (map read-article)
       (map (partial enrich-article cfg))
       (sort #(.compareTo (:date %2) (:date %1)))))

(defn read-me
  [dir]
  (enl/html-resource (io/file (dircat dir "me.html"))))

;; Grouping and Queries

(defn feed-name
  [topic]
  (str "feed-" (if (string/blank? topic) "misc" (string/lower-case topic))))

(defn feed-url
  [site topic]
  (str site "/" (feed-name topic) ".xml"))

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

(defn list-year-groups
  [groups]
  (map (fn [[year articles]]
         (enl/html [:h1 year]
                   [:table (map article-link articles)]))
       groups))

(defn list-topic-groups
  [site groups]
  (map (fn [[topic articles]]
         (enl/html [:h1 [:a {:href (feed-url site topic)}
                         [:image {:src "rss-logo.png"}]]
                    "  "
                    (if (string/blank? topic) "Misc" topic)]
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
                                                                      list-year-groups)))]
    (produce-file (dircat output-dir "by-year.html") template-fn articles)))

(defn produce-by-topic
  [cfg templates-dir output-dir articles]
  (let [template-fn (enl/template (template-html templates-dir) [articles]
                                  [:#content] (apply enl/content (list-topic-groups
                                                                  (:site cfg)
                                                                  (group-by-topic articles))))]
    (produce-file (dircat output-dir "by-topic.html") template-fn articles)))

(defn produce-about
  [templates-dir output-dir me]
  (let [template-fn (enl/template (template-html templates-dir) [_]
                                  [:#content] (enl/content me))]
    (produce-file (dircat output-dir "about.html") template-fn nil)))


;; Feed production


(def render-article (enl/template (enl/html-snippet "<div id='content'>") [a]
                                  [:#content] (apply enl/content (:content a))))

(defn feed-entry
  [site article]
  (let [link (str site "/" (article-href article))]
    (xml/element :entry {}
                 (xml/element :title {} (:title article))
                 (xml/element :link {:href link :type "text/html"})
                 (xml/element :id {} link)
                 (xml/element :updated {} (dt/unparse rfc3339 (:date article)))
                 (xml/element :summary {:type "html"} (->> article render-article (apply str)))
                 (xml/element :author {}
                              (xml/element :name {} (:author article))
                              (xml/element :email {} (:email article))))))


(defn feed
  [cfg [topic articles]]
  (let [entries (map (partial feed-entry (:site cfg)) articles)
        youngest-date (->> articles (sort-by :date) last :date)
        feed-link (feed-url (:site cfg) topic)]
    (xml/element :feed {:xmlns "http://www.w3.org/2005/Atom"}
                 (xml/element :title {} topic)
                 (xml/element :id {} feed-link)
                 (xml/element :link {:href feed-link :rel "self"})
                 (xml/element :link {:href (:site cfg)})
                 (xml/element :updated {} (dt/unparse rfc3339 youngest-date))
                 entries)))


(defn produce-feed
  [cfg output-dir articles]
  (let [groups (group-by-topic articles)]
    (doseq [[topic as] groups]
      (let [target-file (dircat output-dir (str "feed-" (string/lower-case topic) ".xml"))]
        (log "Producing" target-file)
        (spit target-file (xml/emit-str (feed cfg [topic as])))))))


;; Main production process

(defn produce
  [cfg]
  (let [input-dir (io/file (:input.dir cfg))
        output-dir (io/file (:output.dir cfg))
        templates-dir (io/file (dircat input-dir "templates"))
        articles-dir (io/file (dircat input-dir "articles"))
        resources-dir (io/file (dircat input-dir "resources"))]
    (log "All outputs are written to" output-dir)
    (clean-dir output-dir)
    (let [articles (read-articles cfg articles-dir)]
      (produce-articles templates-dir output-dir articles)
      (produce-index templates-dir output-dir articles)
      (produce-by-year templates-dir output-dir articles)
      (produce-by-topic cfg templates-dir output-dir articles)
      (produce-feed cfg output-dir articles)
      (copy-article-resources articles-dir output-dir articles))
    (let [me (read-me articles-dir)]
      (produce-about templates-dir output-dir me))
    (copy-common-resources resources-dir output-dir)
    (log "Done.")))


(defn produce-sample []
  (produce {:input.dir "./sample-data/input"
            :output.dir "./sample-data/output"
            :site "http://my-article.com"
            :email "john.doe@my-articles.com"
            :author "John Doe"}))


(defn -main [& args]
  (let [[params commands usage]
        (cli args
             ["-v" "--verbose" "Write log to stdout." :flag true :default false]
             ["-c" "--config" "Configuration properties file." :default "postatic.properties"])]
    (alter-var-root #'verbose-log (fn [_] (:verbose params)))
    (let [cfg (load-props (:config params))]
      (doseq [c commands]
        (case c
          "produce" (produce cfg)
          "clean" (clean-dir (io/file (:output.dir cfg))))))))


;; To get started in a REPL
#_(def a (read-articles
          {:email "john.doe@my-articles.com"
           :author "John Doe"}
          (io/file "./sample-data/input/articles")))
