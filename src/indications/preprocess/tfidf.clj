(ns indications.preprocess.tfidf
  (:use    [indications.util])
  (:import [java.lang.Double]
           [java.lang.Math]))

"See http://en.wikipedia.org/wiki/Tf%E2%80%93idf"

(def term-freq (atom {}))

(defn tf [term term# token#]
  (let [freq-acc (get @term-freq term 0)]
    (swap! term-freq assoc term (+ freq-acc 1)) 
    (/ term# (+ token# Double/MIN_VALUE))))

(defn idf [doc# docs-with-term#]
  (Math/log10 (/ doc# (+ Double/MIN_VALUE docs-with-term#))))
