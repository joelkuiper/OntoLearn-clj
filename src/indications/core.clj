(ns indications.core
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as strs]
            [taoensso.carmine :as car]
            [indications.preprocess.tokenize :as tok])
  (:import java.io.File
           java.lang.String
           java.util.Arrays
           java.util.concurrent.ConcurrentSkipListSet
           (uk.ac.ebi.ontocat OntologyTerm
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
  (seq (first (map #(get % annotation)
                   (seq (map #(. (ontology :service) getAnnotations (ontology :accession) %) terms))))))
(defn mesh-id [term] 
  (wcar (car/hget "mesh" term)))

(defn- abstract [pmid]
  (wcar (car/hget "abstracts" pmid)))

;(defn- mesh-index [ontology]
  ;"Create MESH->DOID map"
  ;(let [all-terms (seq (. (ontology :service) getAllTerms (ontology :accession)))
        ;size (count all-terms)]
    ;(loop [idx 0 mesh->doid (transient {})] 
      ;(if (>= idx size) 
        ;(persistent! mesh->doid)
        ;(let [doid (.getAccession (nth all-terms idx))
              ;xrefs (annotations ontology "xref" [doid])
              ;mapping (into {} (map #(assoc {} % doid) xrefs))]
          ;(recur (inc idx) (conj! mesh->doid mapping)))))))

(defn create-ontology [file id]
  (let [ontology {:service (FileOntologyService.
                             (java.net.URI. 
                               (.toString (io/as-url file))) id)
                  :accession id}]))
    ;(assoc ontology :mesh->doid (mesh-index ontology))))

(def disease-ontology (atom (create-ontology (io/resource "../resources/HumanDO.obo") "DOID")))
(def tokens (atom (ConcurrentSkipListSet.)))

(defn-memo token-vec []
  (vec @tokens))

;(defn doids [ontology mesh-terms] 
  ;(let [mesh-ids (filter (comp not nil?) (map (fn [x] (mesh-id x)) mesh-terms))
        ;mesh-matches (into {} (map #(get (ontology :mesh->doid) %) mesh-ids))]
    ;(vec mesh-matches)))

(defn- as-num [bool] 
  (if bool 
    1.0
    0.0))

(defn- add-tokens [string]
  (. @tokens addAll string))

(defn emit-feature-vector [abstract] 
  (let [num-features    (.size @tokens)
        feature-vector  (make-array java.lang.String num-features)]
    (dotimes [n num-features]
      (aset feature-vector n (str n ":" (as-num (contains? abstract (nth (token-vec) n))))))
    (strs/join " " (vec feature-vector))))

(defn emit-rows [entry] 
  (when ((comp not nil? val) entry)
    (let [doid (key entry)
          abstract (val entry)]
      (str doid " " (emit-feature-vector abstract) "\n"))))

(defn write-svmlight [entries] 
  (with-open [wtr (io/writer (io/file "dataset/" "data.svm"))]
    (doseq [entry entries] (.write wtr (emit-rows entry)))))

(defn abstract-tokens [pmid]
  (tok/tokenize (wcar (car/hget "abstracts" pmid))))

(defn reverse-map [m]
  (into {} (map (fn [a] (into {} (map (fn [b] (assoc {} b (key a))) (val a)))) m)))

(defn -main [& args]
  (let [doids args
        doid->pmids (into {} (map #(assoc {} % (wcar (car/smembers %))) doids))
        pmids->doid (reverse-map doid->pmids) 
        pmids (flatten (vals doid->pmids))
        abstracts (map (fn [x] (assoc {} (val x) (abstract-tokens (key x)))) pmids->doid)]
    (doall (map add-tokens (vals abstracts)))
    (write-svmlight abstracts)))