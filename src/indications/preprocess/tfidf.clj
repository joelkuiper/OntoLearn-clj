(ns indications.preprocess.tfidf
  (:import [java.lang.Double]
           [java.lang.Math]))

"See http://en.wikipedia.org/wiki/Tf%E2%80%93idf"
(defn tf-idf [term# token# docs-with-term# doc#]
  (let [tf (/ term# (+ token# Double/MIN_VALUE))
        idf (Math/log10 (/ doc# (+ Double/MIN_VALUE docs-with-term#)))]
    (* tf idf)))
