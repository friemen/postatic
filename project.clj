(defproject postatic "1.0.0"
  :description "A generator for static websites made up of article-like postings."
  :url "https://github.com/friemen/postatic"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.2.2"]
                 [enlive "1.1.1"]
                 [clj-time "0.5.0"]]
  :main postatic.core
  :repl-options {:port 9090})