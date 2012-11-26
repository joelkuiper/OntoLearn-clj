(ns indications.import.mesh
  (:use [indications.util]
        [indications.database]
        [indications.ontology])
  (:require [clojure.java.io :as io]
            [clojure.string  :as strs]
            [clj-xpath.core :as xp :only [$x $x:text? $x:text*]]
            [clojure.data.xml :as data.xml]
            [clojure.data.csv :as csv]
            [taoensso.carmine :as car]))

(defn- import-mesh-terms [terms]
  (doseq [term terms]  
    (let [id (first term)
          title (second term)]
      (if (== (wcar (car/hexists "mesh" title) 0))
        (wcar (car/hset "mesh" title id))))))

(defn import-mesh [file]
  (with-open [in-file (io/reader (io/resource file))] 
    (doall (import-mesh-terms (csv/read-csv in-file)))))


