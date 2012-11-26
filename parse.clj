(require '[taoensso.carmine :as car])
(require '[clojure.data.csv :as csv])
(def tree-x (csv/read-csv (io/reader (io/resource "../resources/tree-x.csv")) :separator \;))
(def label-x (csv/read-csv (io/reader (io/resource "../resources/label-x.csv")) :separator \;))

(defn get-labels [csv] 
  (reduce (fn [m [level term id]] (conj m term)) #{} csv))

(def all-labels (get-labels tree-x))
(def top-labels (get-labels label-x))

(defn mapmap [] 
  (persistent!
    (reduce (fn [m [k v]]
              (assoc! m k (f v))) (transient {}) m)))

(def pmid->mesh_disease 
  (into {} (map (fn [pmid] [pmid {:terms (filter #(contains? labels %) (members (str "mesh:" pmid)))}]) (members "mesh_disease"))))

(frequencies (map (fn [[pmid v]] (count (v :terms))) pmid->mesh_disease))

(def tree-levels 
  (reduce 
    (fn [m [cat term id]] (if (nil? (m term)) (assoc m term #{cat}) (assoc m term (conj (m term) cat)))) {} tree-x))

(def top-level (into {} (map (fn [[k v]] [k (reduce into #{} (map tree-levels (:terms v)))]) pmid->mesh_disease)))
(filter #(> (count (val %)) 12) top-level)
(frequencies (map count (vals top-level)))