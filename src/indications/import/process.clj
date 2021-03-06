(ns indications.import.process
  (:use     [indications.util]
            [indications.database]
            [indications.ontology])
  (:require [clojure.java.io :as io]
            [clojure.string  :as strs]
            [clj-xpath.core :as xp :only [$x $x:text? $x:text*]]
            [clojure.data.xml :as data.xml]
            [clojure.data.csv :as csv]
            [taoensso.carmine :as car]))

(defn- import-publication [node] 
  (let [pmid (num (Integer/parseInt (xp/$x:text? ".//MedlineCitation/PMID" node)))]
    (if (= (wcar (car/sismember "all" pmid)) 0)
      (let [mesh-terms (xp/$x:text* ".//MeshHeading//DescriptorName" node)
            ;disease-ids (doids mesh-terms)
            title (apply str (xp/$x:text* ".//ArticleTitle" node))
            abstracts (xp/$x:text* ".//Abstract/AbstractText" node)]
        (wcar ;(doall (map (fn [doid] 
              ;              (car/sadd (str "doid:" doid) pmid)
              ;              (car/sadd "diseases" doid)
              ;              (doall (map #(car/sadd (str "xrefs:" doid) %) (annotations "xref" [doid])))
              ;              (car/sadd (str "pmid:" pmid) doid)) disease-ids))
              (car/hset "abstracts" pmid (str title ",  " (strs/join " " abstracts)))
              (doall (map #(car/sadd (str "mesh:" pmid) %) mesh-terms))
              (doall (map #(car/sadd (str "mesh:" %) pmid) mesh-terms))
              (car/sadd "all" pmid)
              (when-not (empty? mesh-terms) (car/sadd "mesh_annotated" pmid))
              ; (when-not (empty? disease-ids) (car/sadd "annotated" pmid))
              )))))

(defn- import-publications [nodes] 
  (doall (map import-publication nodes)))

(defn- get-publications [file] 
  (->> (:content (data.xml/parse (io/reader file)))
    (filter #(= :PubmedArticle (:tag %)))
    (map data.xml/emit-str)))

(defn- import-files [files] 
  (when (not (empty? files))
    (do 
      (println (str "importing " (first files)))
      (import-publications (get-publications (first files)))
      (recur (rest files)))))

(defn import-directory [directory]
  (let [files (file-seq (io/as-file directory))]
    (if (empty? files)
      (println "Please supply a valid directory with PubMed XML files") 
      (do 
        ;(swap! ontology assoc :mesh->doid (mesh-index @ontology))
        (import-files (rest files))))))

