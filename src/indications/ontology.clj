(ns indications.ontology
  (:require [clojure.java.io :as io])
  (:use     [indications.util]
            [indications.database])
  (:import (uk.ac.ebi.ontocat OntologyTerm
                              OntologyServiceException
                              OntologyService$SearchOptions
                              ols.OlsOntologyService
                              bioportal.BioportalOntologyService
                              file.ReasonedFileOntologyService
                              file.FileOntologyService)))
(defstruct Ontology :service :accession)
(def ontology (atom {})) 

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

(defn create-ontology! [file id & options]
  (let [onto (struct Ontology (ReasonedFileOntologyService.
            (java.net.URI. (.toString (io/as-url file))) id) id)]
    (reset! ontology onto)
    onto))

(defn accessions [terms] 
  (map #(. % getAccession) terms))

(defn- -traverse [doids depth counter direction acc]
  (if (== depth counter) 
    (persistent! acc)
    (let [childs (reduce into [] (map (fn [d] (map (memfn getAccession) (direction d))) doids))]
      (recur (vec childs) depth (inc counter) direction (assoc! acc counter childs)))))

(defn ontological-children [doids depth]
  (-traverse doids depth 0
             (fn [doid] (. (@ontology :service) getChildren (@ontology :accession) doid)) (transient {})))

(defn ontological-parents [doids depth]
  (-traverse doids (java.lang.Math/abs depth) 0 
             (fn [doid] (. (@ontology :service) getParents (@ontology :accession) doid)) (transient {})))

(defn annotations [annotation terms]
  (annotations-int ontology annotation terms))

(defn doids [ontology mesh-terms] 
  (if (nil? (ontology :mesh->doid))
    (println "Ontology did not provide a mesh->doid map, please initialize with mesh-index")
    (let [mesh-ids (filter (comp not nil?) (map (fn [x] (mesh-id x)) mesh-terms))
          mesh-matches (into {} (map #(find (ontology :mesh->doid) %) mesh-ids))]
      (vec (vals mesh-matches)))))

