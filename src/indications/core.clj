(ns indications.core
  (:gen-class :main true)
  (:require [clojure.java.io :as io]
            [clojure.string  :as strs]
            [clj-xpath.core :as xp :only [$x $x:text? $x:text*]]
            [clojure.data.xml :as data.xml]
            [clucy.core :as clucy]
            [clojure.data.csv :as csv]
            [taoensso.carmine :as car])
  (:import (uk.ac.ebi.ontocat OntologyTerm
                              OntologyServiceException
                              OntologyService$SearchOptions
                              ols.OlsOntologyService
                              bioportal.BioportalOntologyService
                              file.FileOntologyService)
           java.net.URLEncoder))

; Set-up redis connection 
(def pool         (car/make-conn-pool)) 
(def spec-server1 (car/make-conn-spec))
(defmacro wcar [& body] `(car/with-conn pool spec-server1 ~@body))

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

(defn-memo accessions [terms] 
  (map #(. % getAccession) terms))

(defn-memo annotations [ontology annotation terms] 
  (flatten (map #(into '() %) (map #(get % annotation) (map (fn [x] (. (ontology :service) getAnnotations (ontology :accession) x)) terms)))))

(defn- ontology-entry [ontology term]
  {:accession (.getAccession term)
   :label (.getLabel term)
   :xrefs (strs/join ", " (annotations ontology "xref" [(.getAccession term)]))
   :synonyms (strs/join ", " (synonyms ontology (.getAccession term)))
   :definition (. (ontology :service) getDefinitions term)})

; Set-up lucene index 
(defn- create-index [ontology]
  (let [lucene (clucy/memory-index)
        all-terms (seq (. (ontology :service) getAllTerms (ontology :accession)))]
    (doseq [term all-terms] (clucy/add lucene (ontology-entry ontology term)))
    lucene))

(defn- mesh-index [ontology]
  "Create MESH->DOID map"
  (let [all-terms (seq (. (ontology :service) getAllTerms (ontology :accession)))
        size (count all-terms)]
    (loop [idx 0 mesh->doid (transient {})] 
      (if (>= idx size) 
        (persistent! mesh->doid)
        (let [doid (.getAccession (nth all-terms idx))
              xrefs (annotations ontology "xref" [doid])
              mapping (into {} (map #(assoc {} % doid) xrefs))]
        (recur (inc idx) (conj! mesh->doid mapping)))))))

(defn create-ontology [file id]
  (let [ontology {:service (FileOntologyService.
                            (new java.net.URI 
                                 (.toString (io/as-url file))) id)
                 :accession id}]
      (assoc ontology :mesh->doid (mesh-index ontology) :lucene (create-index ontology))))

(def disease-ontology (atom (create-ontology (io/resource "../resources/HumanDO.obo") "DOID")))
;(def disease-ontology)

(defn search [ontology]
  (fn [terms]
    (map :accession (flatten (map #(clucy/search (ontology :lucene) % 20) terms)))))

;(def search-file (search disease-ontology))

(defn mesh-id [term] 
  (wcar (car/hget "mesh" term)))

(defn doids [ontology mesh-terms] 
  (let [mesh-ids (filter (comp not nil?) (map (fn [x] (mesh-id x)) mesh-terms))
        mesh-matches (into {} (map #(find (ontology :mesh->doid) %) mesh-ids))]
    (vec (vals mesh-matches))))

(defn import-publication [node] 
  (let [pmid (num (Integer/parseInt (xp/$x:text? ".//MedlineCitation/PMID" node)))]
    (if (= (wcar (car/sismember "all" pmid)) 0)
      (let [mesh-terms (xp/$x:text* ".//MeshHeading//DescriptorName" node)
            disease-ids (doids @disease-ontology mesh-terms) 
            abstracts (xp/$x:text* ".//Abstract/AbstractText" node)]
        (do (wcar (doall (map (fn [doid] 
                                (do 
                                  (car/sadd  doid pmid)
                                  (car/sadd "diseases" doid)
                                  (doall (map #(car/sadd (str "mesh_" pmid) %) mesh-terms))
                                  (doall (map #(car/sadd (str "xrefs_" doid) %) (annotations @disease-ontology "xref" [doid])))
                                  (car/sadd (str "pmid_" pmid) doid))) disease-ids))
                  (car/hset "abstracts" pmid (strs/join " " abstracts))
                  (car/sadd "all" pmid)
                  (if (not (empty? mesh-terms)) (car/sadd "mesh_annotated" pmid))
                  (if (not (empty? disease-ids)) (car/sadd "annotated" pmid))))))))

(defn- import-publications [nodes] 
  (doall (pmap import-publication nodes)))

(defn- get-publications [file] 
  (->> (:content (data.xml/parse (io/input-stream file)))
    (filter #(= :PubmedArticle (:tag %)))
    (map data.xml/emit-str)))

(defn import-files [files] 
  (if (empty? files) 
    true
    (do 
      (println (str "importing " (first files)))
      (import-publications (get-publications (first files)))
      (recur (rest files)))))

;Code for importing mesh to term mapping in Redis
(defn- import-mesh-terms [terms]
  (if (empty? terms)
    true
    (let [id (first (first terms))
          title (second (first terms))]
      (if (= (wcar (car/hexists "mesh" title)) 0)
        (wcar (car/hset "mesh" title id)))
      (recur (rest terms)))))

(defn import-mesh [file]
  (with-open [in-file (io/reader (io/resource file))] 
    (doall (import-mesh-terms (csv/read-csv in-file)))))

(defn -main [& args]
  (let [files (file-seq (io/as-file (first args)))]
    (if (empty? files)
      (println "Please supply a valid directory with PubMed XML files")
      (do 
        (import-files (rest files))))))

