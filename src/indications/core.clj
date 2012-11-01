(ns indications.core
  (:gen-class :main true)
  (:require  [clojure.java.io :as io]
             [clojure.string  :as strs]
             [clj-xpath.core :as xp :only [$x $x:text? $x:text*]]
             [clojure.data.xml :as data.xml]
             [taoensso.carmine :as car])
  (:import (uk.ac.ebi.ontocat OntologyTerm
                              OntologyServiceException
                              OntologyService$SearchOptions
                              ols.OlsOntologyService
                              bioportal.BioportalOntologyService
                              file.ReasonedFileOntologyService
                              virtual.CachedServiceDecorator)
            java.net.URLEncoder)
)

; Set-up redis connection 
(def pool         (car/make-conn-pool)) 
(def spec-server1 (car/make-conn-spec))
(defmacro wcar [& body] `(car/with-conn pool spec-server1 ~@body))

(defonce file-ontology
  {:service (new ReasonedFileOntologyService 
                 (new java.net.URI 
                      (.toString (io/as-url (io/resource "../resources/HumanDO.obo")))) "DOID")
   :accession "DOID"})

;(def bioportal
;  {:service (new BioportalOntologyService "6c830f6b-6cfc-435c-b8a7-a289333d25cb")
;   :accession "1009"})

;(def search-ontology-memo 
;  (memoize search-ontology))

; defn-memo by Chouser:
(defmacro defn-memo
  "Just like defn, but memoizes the function using clojure.core/memoize"
  [fn-name & defn-stuff]
  `(do
     (defn ~fn-name ~@defn-stuff)
     (alter-var-root (var ~fn-name) memoize)
     (var ~fn-name)))


(defn synonyms [ontology accession]
  (seq (. (ontology :service) getSynonyms (ontology :accession) accession)))

(defn label [term]
  (. term getLabel))
  
(defn-memo accessions [terms] 
 (map #(. % getAccession) terms))

(defn ontology-assoc [ontology term]
  (let [accession (first (accessions [term]))
        label     (label term)
        synonyms  (synonyms ontology accession)
        terms     (cons label synonyms)]
    (into {} (map (fn [term] {(strs/lower-case term) accession}) terms))))
  
(defn ontology-table [ontology]
  (let [all-terms  (seq (. (ontology :service) getAllTerms (ontology :accession)))
        size       (count all-terms)]
    (loop [i 0 m (transient {})]
      (if (< i size)
        (recur (inc i) (conj! m (ontology-assoc ontology (nth all-terms i))))
        (persistent! m)))))

(defn search [ontology-table]
  (fn [terms]
    (into {} (map (fn [term] (apply hash-map (remove nil? (find ontology-table (strs/lower-case term))))) terms))))

(def search-file (search (ontology-table file-ontology)))

(defn import-publication [node] 
  (let [pmid (xp/$x:text? ".//MedlineCitation/PMID" node)]
    (if (= (wcar (car/sismember "all" pmid)) 0)
      (let [mesh-terms (xp/$x:text* ".//MeshHeading//DescriptorName" node)
            disease-ids (vec (vals (search-file mesh-terms)))
            abstracts (xp/$x:text* ".//Abstract/AbstractText" node)
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

