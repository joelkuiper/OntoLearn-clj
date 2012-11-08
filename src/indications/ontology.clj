(ns indications.ontology
  (:require [clojure.java.io :as io])
  (:use     [indications.util]
            [indications.database])
  (:import (uk.ac.ebi.ontocat OntologyTerm
                              OntologyServiceException
                              OntologyService$SearchOptions
                              ols.OlsOntologyService
                              bioportal.BioportalOntologyService
                              file.FileOntologyService)))

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
  (let [ontology {:service (FileOntologyService.
                             (java.net.URI. (.toString (io/as-url file))) id)
                  :accession id}]
    (assoc ontology :mesh->doid (mesh-index ontology))))

(def ontology (create-ontology (io/resource "../resources/HumanDO.obo") "DOID"))

(defn- synonyms-int [ontology accession]
  (seq (. (ontology :service) getSynonyms (ontology :accession) accession)))

(defn synonyms [accession] 
  (synonyms-int ontology accession))

(defn accessions [terms] 
  (map #(. % getAccession) terms))

(defn annotations [annotation terms]
  (annotations-int ontology annotation terms))

(defn doids-int [ontology mesh-terms] 
  (let [mesh-ids (filter (comp not nil?) (map (fn [x] (mesh-id x)) mesh-terms))
        mesh-matches (into {} (map #(find (ontology :mesh->doid) %) mesh-ids))]
    (vec (vals mesh-matches))))

(defn doids [mesh-terms]
  (doids-int ontology mesh-terms))
