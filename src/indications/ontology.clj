(ns indications.ontology
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:use     [indications.util]
            [indications.database])
  (:import (uk.ac.ebi.ontocat OntologyTerm
                              OntologyServiceException
                              OntologyService$SearchOptions
                              ols.OlsOntologyService
                              bioportal.BioportalOntologyService
                              file.ReasonedFileOntologyService)))

(defn- annotations-int [ontology annotation terms] 
  (seq (first (map #(get % annotation)
                   (seq (map #(. (ontology :service) getAnnotations (ontology :accession) %) terms))))))

(defn mesh-index [ontology]
  "Create MESH->DOID map"
  (let [all-terms (seq (. (ontology :service) getAllTerms (ontology :accession)))
        size (count all-terms)]
    (loop [idx 0 mesh->doid (transient {})] 
      (if (>= idx size) 
        (persistent! mesh->doid)
        (let [doid (.getAccession (nth all-terms idx))
              xrefs (annotations-int ontology "xref" [doid])
              mapping (into {} (map #(assoc {} % doid) xrefs))]
          (recur (inc idx) (conj! mesh->doid mapping)))))))

(defn create-ontology [file id & options]
  (let [ontology {:service (ReasonedFileOntologyService.
                             (java.net.URI. (.toString (io/as-url file))) id)
                  :accession id}]
    ontology))

(def ontology (atom (create-ontology (io/resource "../resources/HumanDO.obo") "DOID")))

(defn- synonyms-int [ontology accession]
  (seq (. (ontology :service) getSynonyms (ontology :accession) accession)))

(defn synonyms [accession] 
  (synonyms-int ontology accession))

(defn accessions [terms] 
  (map #(. % getAccession) terms))

(defn children [doids depth acc]
  (if (== depth 0) 
    (flatten (persistent! acc))
    (let [childs (reduce into '() (map (fn [d] (map (memfn getAccession) (.getChildren (@ontology :service) (@ontology :accession) d))) doids))]
      (recur (vec childs) (dec depth) (conj! acc childs)))))

(defn annotations [annotation terms]
  (annotations-int ontology annotation terms))

(defn doids [ontology mesh-terms] 
  (if (nil? (ontology :mesh->doid))
    (println "Ontology did not provide a mesh->doid map, please initialize with mesh-index")
    (let [mesh-ids (filter (comp not nil?) (map (fn [x] (mesh-id x)) mesh-terms))
          mesh-matches (into {} (map #(find (ontology :mesh->doid) %) mesh-ids))]
      (vec (vals mesh-matches)))))

