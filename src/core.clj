(ns ml-indications.core
  (:import (uk.ac.ebi.ontocat OntologyServiceException
                              OntologyTerm
                              OntologyService$SearchOptions
                              file.FileOntologyService)
           java.net.URLEncoder)
  (:require [clojure.java.io :as io]
            [clj-http.client :as client]
            [clojure.string :as str]
            [clj-xpath.core :as xp :only [$x* $x:text* $x:text?]])
)

;(def human-do 
;  (new FileOntologyService 
;       (new java.net.URI 
;            (.toString (io/as-url (io/resource "resources/HumanDO.obo"))))
;  )
;)
;
;(defn search [terms]
;  (map (fn [term] 
;      (. human-do searchAll term (make-array OntologyService$SearchOptions 0))) terms)
;)

(def pubmed "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/")

(defn to-url [options] 
    (str/join "&" (map (fn [[k v]] (str (name k) "=" v)) options)))

(defn pubmed-search
  "Queries PubMed and returns a set of PMIDs and a reference to the saved search"
  [q]
  (let [api (str pubmed "esearch.fcgi?")
        options (to-url {:term (URLEncoder/encode q) 
                         :usehistory "y"
                         :type "count"})
        url (str api options)
        results (client/get url)]
  {:data (results :body)
   :url url
   :QueryKey (xp/$x:text "/eSearchResult/QueryKey" (results :body))
   :WebEnv (xp/$x:text "/eSearchResult/WebEnv" (results :body))
   :Total (Integer/parseInt (xp/$x:text "/eSearchResult/Count" (results :body)))}))

(defn pubmed-fetch 
  "Fetches a range of abstracts based upon a saved search"
  [web-env query-key start end]
  (let [api (str pubmed "efetch.fcgi?")
        options (to-url {:db "pubmed"
                     :query_key query-key
                     :WebEnv web-env
                     :retmode "xml"
                     :retstart start
                     :retmax 10000})
        url (str api options)
        results (client/get url {:socket-timeout 3000 :conn-timeout 3000})]
    (results :body))) 

(defn pubmed-error? [url]
  (println url)
  (if (not(nil?(xp/$x:text? "/eSearchResult/ERROR" ((client/get url) :body))))
    (do 
      (println "Error found")
      true)
    false))

(defn save-all
  "Saves the full xml records to xml files in the application directory"
  [dir q]
  (let [results (pubmed-search q)
        web-env (results :WebEnv)
        query-key (results :QueryKey)
        total (results :Total)
        step 10000]
    (loop [cnt 0]
      (while (pubmed-error? (str (results :url) "&" (to-url {"WebEnv" web-env "query_key" query-key}))) (Thread/sleep 500))
      (println (str query-key " " web-env " " total " " (* cnt step)))
        (if (>= (* cnt step) total)
          true
          (do (spit (str dir "/" cnt ".xml") (pubmed-fetch web-env query-key (* cnt step) (* (inc cnt) step)))
            (recur (inc cnt)))))))




                     