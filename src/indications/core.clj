(ns indications.core
  (:gen-class :main true)
  (:require  [clojure.java.io :as io]
             [clj-xpath.core :as xp :only [$x $x:text? $x:text*]]
             [clojure.data.xml :as data.xml]
             [taoensso.carmine :as car])
  (:import (uk.ac.ebi.ontocat OntologyServiceException
                              OntologyTerm
                              OntologyService$SearchOptions
                              ols.OlsOntologyService
                              bioportal.BioportalOntologyService
                              virtual.CachedServiceDecorator)
            java.net.URLEncoder)
)

; Set-up redis connection 
(def pool         (car/make-conn-pool)) 
(def spec-server1 (car/make-conn-spec))
(defmacro wcar [& body] `(car/with-conn pool spec-server1 ~@body))

;(def disease-ontology 
;  (new FileOntologyService 
;                 (new java.net.URI 
;                      (.toString (io/as-url (io/resource "../resources/HumanDO.obo"))))))

(def human-disease-ontology "1009") 

(def bioportal
  (new BioportalOntologyService "6c830f6b-6cfc-435c-b8a7-a289333d25cb"))

(def ols
  (new OlsOntologyService))

(defn search-ontology [service ontology terms]
  (let [options (make-array OntologyService$SearchOptions 1)]
    (aset options 0 (. OntologyService$SearchOptions valueOf "EXACT"))
    (seq (reduce concat (map (fn [term] (. service searchOntology ontology term options)) terms))))
)

(def search-ontology-memo (memoize search-ontology))

(defn accessions [terms] 
 (map #(. % getAccession) terms))

(def accessions-mem
  (memoize accessions))

(defn annotations [service ontology annotation terms] 
  (map #(get % annotation)
       (seq (map #(. service getAnnotations ontology %) terms))))

(defn import-publication [node] 
  (let [pmid (xp/$x:text? ".//MedlineCitation/PMID" node)]
    (if (= (wcar (car/sismember "all" pmid)) 0)
      (let [mesh-terms (xp/$x:text* ".//MeshHeading//DescriptorName" node)
            disease-terms (search-ontology-memo bioportal human-disease-ontology mesh-terms)
            abstracts (xp/$x:text* ".//Abstract/AbstractText" node)
            disease-ids (vec (accessions-mem disease-terms))
          ]
      (do (wcar (doall (map #(car/sadd pmid %1) disease-ids))
                (doall (map #(car/hset "abstracts" pmid %1) abstracts))
                (doall (map #(car/sadd (str "disease-" disease-ids) %1) pmid))
                (doall (map #(car/sadd "diseases" %1) disease-ids))
                (car/sadd "all" pmid)
                (if (not (empty? disease-ids)) (car/sadd "annotated" pmid))
                ))
      )
    )
))

(defn import-publications [nodes] 
  (doall (map import-publication nodes)))

(defn process-publication [publication]
  (data.xml/emit-str publication))

(defn get-publications [file] 
    (->> (:content (data.xml/parse (io/reader file)))
       (filter #(= :PubmedArticle (:tag %)))
       (map process-publication)))

(defn import-files [files] 
  (if (empty? files) 
    true
    (do 
      (println (str "importing " (first files)))
      (import-publications (get-publications (first files)))
      (recur (rest files)))))

(defn -main [& args]
  (let [files (file-seq (io/as-file (first args)))]
    (if (empty? files)
      (println "Please supply a valid directory with PubMed XML files") 
      (import-files (rest files)))))

