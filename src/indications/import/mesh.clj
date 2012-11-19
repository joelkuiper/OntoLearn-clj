(ns indications.import.mesh) 

(defn- import-mesh-terms [terms]
  (doseq [term terms]  
    (let [id (first term)
          title (second term)]
      (if (== (wcar (car/hexists "mesh" title) 0))
        (wcar (car/hset "mesh" title id))))))

(defn import-mesh [file]
  (with-open [in-file (io/reader (io/resource file))] 
    (doall (import-mesh-terms (csv/read-csv in-file)))))


