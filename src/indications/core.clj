(ns indications.core
  (:require [clojure.java.io :as io]
            [clojure.string  :as strs]
            [clojure.data.csv :as csv]
            [taoensso.carmine :as car])
  (:import  java.io.File))

; Set-up redis connection 
(def pool         (car/make-conn-pool)) 
(def spec-server1 (car/make-conn-spec))
(defmacro wcar [& body] `(car/with-conn pool spec-server1 ~@body))

(def root-dir "dataset/supervised-unaided/")

(defn write-abstract [dir pmid abstract] 
  (with-open [wtr (io/writer (io/file dir (str pmid ".dat")))]
    (.write wtr abstract)))

(defn write-abstracts [entry]
  (let [dir (str root-dir (key entry) "/")]
    (.mkdirs (File. dir))
    (map (fn [pmid] (write-abstract dir pmid (wcar (car/hget "abstracts" pmid)))) (val entry))))

(defn -main [& args]
  (let [diseases        (wcar (car/smembers "diseases"))
        disease->pmids  (zipmap diseases (map #(wcar (car/smembers %)) diseases))]
    (map write-abstracts disease->pmids)))