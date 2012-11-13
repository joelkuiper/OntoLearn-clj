(ns indications.preprocess.tfidf
  (:import [java.lang.Double]
            [java.lang.Math]))

(defn tf-idf [doc#]
  "See http://en.wikipedia.org/wiki/Tf%E2%80%93idf"
  (fn [term# token# docs-with-term#]
    (let [tf (/ term# (+ token# Double/MIN_VALUE))
          idf (Math/log10 (/ doc# (+ Double/MIN_VALUE docs-with-term#)))]
      (* tf idf))))
