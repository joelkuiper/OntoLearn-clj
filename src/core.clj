(ns ml-indications.core
  (:require  [clojure.java.io :as io]
             [clj-xpath.core :as xp :only [$x $x:text? $x:text*]]
             [taoensso.carmine :as car])
  (:import (uk.ac.ebi.ontocat OntologyServiceException
                              OntologyTerm
                              OntologyService$SearchOptions
                              file.FileOntologyService
                              virtual.CachedServiceDecorator)
            java.net.URLEncoder)
)

; Set-up redis connection 
(def pool         (car/make-conn-pool)) 
(def spec-server1 (car/make-conn-spec))
(defmacro wcar [& body] `(car/with-conn pool spec-server1 ~@body))

(def disease-ontology 
  (let [fos (new FileOntologyService 
                 (new java.net.URI 
                      (.toString (io/as-url (io/resource "../resources/HumanDO.obo")))))
        cos (. CachedServiceDecorator getService fos)]
    cos
  )
)

(defn search-ontology [ontology terms]
  (let [options (make-array OntologyService$SearchOptions 1)]
    (aset options 0 (. OntologyService$SearchOptions valueOf "EXACT"))
    (seq (reduce concat (map (fn [term] (. ontology searchAll term options)) terms))))
)

(defn accessions [terms] 
 (map #(. % getAccession) terms))

(defn annotations [annotation terms] 
  (map #(get % annotation)
       (seq (map #(. disease-ontology getAnnotations %) terms))))

(defn import-publication [node] 
  (let [pmid (xp/$x:text? "./MedlineCitation/PMID" node)]
    (if (= (wcar (car/sismember "pmids" pmid)) 0)
		  (let [mesh-terms (xp/$x:text* ".//MeshHeading//DescriptorName" node)
		        disease-terms (search-ontology disease-ontology mesh-terms)
            abstracts (xp/$x:text* ".//Abstract/AbstractText" node)
		        disease-ids (vec (accessions disease-terms))
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
  (pmap import-publication nodes))

(defn get-publications [file] 
  (import-publications (xp/$x "/PubmedArticleSet/PubmedArticle" (slurp (io/as-file file)))))

(defn import-files [files] 
  (pmap get-publications files))