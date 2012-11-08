(ns indications.util)

; defn-memo by Chouser:
(defmacro defn-memo
  "Just like defn, but memoizes the function using clojure.core/memoize"
  [fn-name & defn-stuff]
  `(do
     (defn ~fn-name ~@defn-stuff)
     (alter-var-root (var ~fn-name) memoize)
     (var ~fn-name)))

(defn deep-reverse-map [m]
  (into {} (map (fn [a] (into {} (map (fn [b] (assoc {} b (key a))) (val a)))) m)))
