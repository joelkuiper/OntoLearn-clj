(ns indications.import.import
  (:require [clojure.java.io :as io]
            [clojure.string  :as strs]
            [clojure.indications.core]
            [clj-xpath.core :as xp :only [$x $x:text? $x:text*]]
            [clojure.data.xml :as data.xml]
            [clojure.data.csv :as csv]
            [taoensso.carmine :as car]))
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

(defn doids [ontology mesh-terms] 
  (let [mesh-ids (filter (comp not nil?) (map (fn [x] (mesh-id x)) mesh-terms))
        mesh-matches (into {} (map #(get (ontology :mesh->doid) %) mesh-ids))]
    (vec mesh-matches)))

(defn import-publication [node] 
  (let [pmid (num (Integer/parseInt (xp/$x:text? ".//MedlineCitation/PMID" node)))]
    (if (= (wcar (car/sismember "all" pmid)) 0)
      (let [mesh-terms (xp/$x:text* ".//MeshHeading//DescriptorName" node)
            disease-ids (doids @disease-ontology mesh-terms)
            abstracts (xp/$x:text* ".//Abstract/AbstractText" node)]
        (do (wcar (doall (map (fn [doid] 
                                (do 
                                  (car/sadd (str "doid_" doid) pmid)
                                  (car/sadd "diseases" doid)
                                  (doall (map #(car/sadd (str "mesh_" pmid) %) mesh-terms))
                                  (doall (map #(car/sadd (str "xrefs_" doid) %) (annotations @disease-ontology "xref" [doid])))
                                  (car/sadd (str "pmid_" pmid) doid))) disease-ids))
                  (car/hset "abstracts" pmid (strs/join " " abstracts))
                  (car/sadd "all" pmid)
                  (if (not (empty? mesh-terms)) (car/sadd "mesh_annotated" pmid))
                  (if (not (empty? disease-ids)) (car/sadd "annotated" pmid))
                  ))))))

(defn import-publications [nodes] 
  (doall (map import-publication nodes)))

(defn get-publications [file] 
  (->> (:content (data.xml/parse (io/reader file)))
    (filter #(= :PubmedArticle (:tag %)))
    (map data.xml/emit-str)))

(defn import-files [files] 
  (if (empty? files) 
    true
    (do 
      (println (str "importing " (first files)))
      (import-publications (get-publications (first files)))
      (recur (rest files)))))

; Code for importing mesh to term mapping in Redis
(defn import-mesh-terms [terms]
  (when (seq? terms)
    (let [id (first (first terms))
          title (second (first terms))]
      (if (= (wcar (car/hexists "mesh" title) 0))
        (wcar (car/hset "mesh" title id)))
      (recur (rest terms)))))

(defn import-mesh [file]
  (with-open [in-file (io/reader (io/resource file))] 
    (doall (import-mesh-terms (csv/read-csv in-file)))))

(defn -main [& args]
  (let [files (file-seq (io/as-file (first args)))]
    (def disease-ontology (atom (assoc @disease-ontology :mesh->doid (mesh-index @disease-ontology))))
    (if (empty? files)
      (println "Please supply a valid directory with PubMed XML files") 
      (do 
        (import-files (rest files))))))

