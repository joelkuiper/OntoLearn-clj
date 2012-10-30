(ns ml-indications.core
  (:require  [clojure.java.io :as io]
             [clj-xpath.core :as xp :only [$x $x:text? $x:text*]]
             [taoensso.carmine :as car])
  (:import (uk.ac.ebi.ontocat OntologyServiceException
                              OntologyTerm
                              OntologyService$SearchOptions
                              file.FileOntologyService)
            java.net.URLEncoder)
)

; Set-up redis connection 
(def pool         (car/make-conn-pool)) 
(def spec-server1 (car/make-conn-spec))
(defmacro wcar [& body] `(car/with-conn pool spec-server1 ~@body))

(def disease-ontology 
  (new FileOntologyService 
       (new java.net.URI 
            (.toString (io/as-url (io/resource "../resources/HumanDO.obo"))))
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

(defn parse-publication [node] 
  (let [pmid (xp/$x:text? ".//PMID" node)
        db-diseases (wcar (car/smembers pmid))]
    (if (empty? db-diseases)
		  (let [mesh-terms (xp/$x:text* ".//MeshHeading//DescriptorName" node)
		        disease-terms (search-ontology disease-ontology mesh-terms)
		        disease-ids (vec (accessions disease-terms))]
      (do (wcar (doall (map #(car/sadd pmid %1) disease-ids))
                (car/sadd "pmids" pmid))
        {:pmid pmid 
         :disease disease-ids})
      )
    {:pmid pmid 
     :disease db-diseases}
    )
))