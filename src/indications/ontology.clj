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

(defn accession [^OntologyTerm term] (.getAccession term))

(defn accessions [terms] 
  (map #(accession %) terms))

(defn- -traverse [doids depth counter direction acc]
  (if (== depth (- counter 1)) 
    (persistent! acc)
    (let [childs (reduce into [] (map (fn [d] (map accession (direction d))) doids))]
      (recur (vec childs) depth (inc counter) direction (assoc! acc counter childs)))))

(defn ontological-children [doids depth]
  (assoc (-traverse doids depth 1
             (fn [doid] (. ^ReasonedFileOntologyService (@ontology :service) getChildren (@ontology :accession) doid)) (transient {})) 0 doids))

(defn ontological-parents [doids depth]
  (assoc (-traverse doids (abs depth) 1 
             (fn [doid] (. ^ReasonedFileOntologyService (@ontology :service) getParents (@ontology :accession) doid)) (transient {})) 0 doids))

(defn annotations [annotation terms]
  (annotations-int @ontology annotation terms))

(defn doids [mesh-terms] 
  (if (nil? (@ontology :mesh->doid))
    (println "Ontology did not provide a mesh->doid map, please initialize with mesh-index")
    (let [mesh-ids (filter (comp not nil?) (map (fn [x] (mesh-id x)) mesh-terms))
          mesh-matches (into {} (map #(find (@ontology :mesh->doid) %) mesh-ids))]
      (vec (vals mesh-matches)))))

