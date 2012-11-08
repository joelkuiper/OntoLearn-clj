(ns indications.database
  (:require [taoensso.carmine :as car]))

; Set-up redis connection 
(def pool         (car/make-conn-pool)) 
(def spec-server1 (car/make-conn-spec))
(defmacro wcar [& body] `(car/with-conn pool spec-server1 ~@body))

(defn mesh-id [term] 
  (wcar (car/hget "mesh" term)))

(defn abstract [pmid]
  (wcar (car/hget "abstracts" pmid)))

(defn members [field] 
  (wcar (car/smembers field)))
