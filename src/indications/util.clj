(ns indications.util)

; defn-memo by Chouser:
(defmacro defn-memo
  "Just like defn, but memoizes the function using clojure.core/memoize"
  [fn-name & defn-stuff]
  `(do
     (defn ~fn-name ~@defn-stuff)
     (alter-var-root (var ~fn-name) memoize)
     (var ~fn-name)))

(defn invert-list-entry [entry] (map (fn [k] [k (key entry)]) (val entry)))

(defn deep-invert-map
  [list-map]
  (let [values (mapcat invert-list-entry list-map)]
    (loop [entries values acc (transient {})]
      (if (empty? entries) 
        (persistent! acc)
      (let [entry (first entries) 
            k (first entry)
            v (second entry)
            curr (acc k)]
        (if (nil? curr) 
          (recur (rest entries) (assoc! acc k [v]))
          (recur (rest entries) (assoc! acc k (conj curr v)))))))))

