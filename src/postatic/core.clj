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

;; ------------------------------------------------------------------------------------------
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
  (let [srcs   (-> nodes
                   (enl/select [#{:img :source}])
                   (->> (map #(-> % :attrs :src))))
        hrefs (-> nodes
                  (enl/select [[:a (enl/attr? :href)]])
                  (->> (map #(-> % :attrs :href))))]
    (->> (concat srcs hrefs)
         (filter local-ref?))))

(defn read-article
  [file]
  (let [html   (enl/html-resource file)
        topics (-> html (string-of-content [:head :topics]) (string/split #"\s"))
        date   (-> html (string-of-content [:head :date]))
        title  (-> html (string-of-content [:h1]))
        refs   (-> html local-resources)]
    (log "Found:" title date topics)
    {:title title
     :date (dt/parse ddmmyyyy date)
     :topics (if (string/blank? (apply str topics)) ["Misc"] topics)
     :file file
     :filerefs refs
     :content (-> html (enl/select [:#page]) first :content)}))

(defn perma-href
  [article]
  (-> article :file .getName (string/escape {\space "%20"})))



(defn enrich-article
  [cfg article]
  (let [date     (dt/unparse ddmmyyyy (:date article))
        title    (:title article)
        e-title  (string/escape title {\space "%20"})
        p-href   (perma-href article)
        full-url (str (:site cfg) "/" p-href)
        t-href   (str "http://twitter.com/intent/tweet?text="
                      e-title "%20-%20" full-url)
        f-href   (str "https://www.facebook.com/sharer.php?u=" full-url)
        l-href   (str "http://www.linkedin.com/shareArticle?mini=true&url=" full-url
                      "&title=" (string/escape title {\space "%20"}))
        x-href   (str "https://www.xing.com/social_plugins/share?url=" full-url)]
    (assoc article
      :content (concat (enl/at (:content article)
                               [:h1] (enl/substitute (enl/html
                                                      [:a {:name p-href}]
                                                      [:h1 title]
                                                      date " " [:a {:href p-href} "Permalink"] [:p])))
                       (enl/html [:div {:class "share-bar"}
                                  [:a {:target "_blank" :href x-href}
                                   [:i {:class "xing-button icon-xing"}]]
                                  [:a {:target "_blank" :href l-href}
                                   [:i {:class "linkedin-button icon-linkedin"}]]
                                  [:a {:target "_blank" :href t-href}
                                   [:i {:class "twitter-button icon-twitter"}]]
                                  [:a {:target "_blank" :href f-href}
                                   [:i {:class "facebook-button icon-facebook"}]]]))
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
  (-> (enl/html-resource (io/file (dircat dir "me.html")))
      (enl/select [:#page]) first :content))


(defn read-privacy-policy
  [dir]
  (-> (enl/html-resource (io/file (dircat dir "privacy-policy.html")))
      (enl/select [:#page]) first :content))


;; ------------------------------------------------------------------------------------------
;; Grouping and Queries

(defn feed-name
  [topic]
  (str "feed-" (if (string/blank? topic) "misc" (string/lower-case topic))))

(defn feed-url
  [site topic]
  (str site "/" (feed-name topic) ".xml"))

(defn recent
  [articles]
  (take 5 articles))

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

;; ------------------------------------------------------------------------------------------
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
          :when (and (.isFile src) (not (article-file? src)))]
    (log src " -> " dst)
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

;; ------------------------------------------------------------------------------------------
;; Html Production using templates

(defn article-link
  [article]
  [:tr
   [:td (dt/unparse ddmmyyyy (:date article))]
   [:td [:a {:href (perma-href article)} (:title article)]]])

(defn list-year-groups
  [groups]
  (->> groups
       (sort-by first)
       reverse
       (map (fn [[year articles]]
              (enl/html [:h1 year]
                        [:table (map article-link articles)])))))

(defn list-topic-groups
  [site groups]
  (->> groups
       (sort-by first)
       (map (fn [[topic articles]]
              (enl/html [:h1 [:a {:href (feed-url site topic)}
                              [:i {:style "color:#FEA501" :class "icon-rss-squared"}]]
                         "  "
                         topic]
                        [:table (map article-link articles)])))))

(defn produce-index
  [templates-dir output-dir articles]
  (let [template-fn (enl/template (template-html templates-dir) [articles]
                                  [:#content] (apply enl/content (->> articles
                                                                      recent
                                                                      (map :content)
                                                                      (interpose (enl/html [:p] [:hr])))))]
    (produce-file (dircat output-dir "index.html") template-fn articles)))

(defn produce-articles
  [cfg templates-dir output-dir articles]
  (let [template-fn (enl/template (template-html templates-dir) [a]
                                  [:head :title] (enl/content (str (:title a) " - " (:title cfg)))
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


(defn produce-privacy-policy
  [templates-dir output-dir pp]
  (let [template-fn (enl/template (template-html templates-dir) [_]
                                  [:#content] (enl/content pp))]
    (produce-file (dircat output-dir "privacy-policy.html") template-fn nil)))

;; Feed production


(def render-article (enl/template (enl/html-snippet "<div id='content'>") [a]
                                  [:#content] (apply enl/content (:content a))))

(defn feed-entry
  [site article]
  (let [link (str site "/" (perma-href article))]
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
  (let [entries       (map (partial feed-entry (:site cfg)) articles)
        youngest-date (->> articles (sort-by :date) last :date)
        feed-link     (feed-url (:site cfg) topic)]
    (xml/element :feed {:xmlns "http://www.w3.org/2005/Atom"}
                 (xml/element :title {} topic)
                 (xml/element :id {} feed-link)
                 (xml/element :link {:href feed-link :rel "self"})
                 (xml/element :link {:href (:site cfg)})
                 (xml/element :updated {} (dt/unparse rfc3339 youngest-date))
                 entries)))


(defn produce-feeds
  [cfg output-dir articles]
  (letfn [(produce [topic as]
            (let [target-file (feed-url output-dir topic)]
              (log "Producing" target-file)
              (spit target-file (xml/emit-str (feed cfg [topic as])))))]
    (produce "All" articles)
    (doseq [[topic as] (group-by-topic articles)]
      (produce topic as))))


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
      (produce-articles cfg templates-dir output-dir articles)
      (produce-index templates-dir output-dir articles)
      (produce-by-year templates-dir output-dir articles)
      (produce-by-topic cfg templates-dir output-dir articles)
      (produce-feeds cfg output-dir articles)
      (copy-article-resources articles-dir output-dir articles))
    (let [me (read-me articles-dir)]
      (produce-about templates-dir output-dir me))
    (let [privacy-policy (read-privacy-policy articles-dir)]
      (produce-privacy-policy templates-dir output-dir privacy-policy))
    (copy-common-resources resources-dir output-dir)
    (log "Done.")))


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


;; ------------------------------------------------------------------------------------------
;; Tools for the REPL

(defn produce-sample []
  (produce {:input.dir "./sample-data/input"
            :output.dir "./sample-data/output"
            :title "my-articles"
            :site "http://my-article.com"
            :email "john.doe@my-articles.com"
            :author "John Doe"}))

#_ (def render-article
  (enl/template (template-html "./sample-data/input/templates") [a]
                [:#content] (apply enl/content (:content a))))


;; read some sample articles
#_ (def as (vec (read-articles
              {:email "john.doe@my-articles.com"
               :author "John Doe"}
              (io/file "./sample-data/input/articles"))))
#_ (get-in as [0 :content])

#_ (render-article (as 0))
