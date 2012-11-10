(ns indications.preprocess.tokenize
  (:require [clojure.string :as strs])
  (:import (org.apache.lucene.analysis.standard StandardTokenizer 
                                                StandardAnalyzer)
           java.io.StringReader
           java.util.concurrent.ConcurrentSkipListSet
           java.util.Arrays
           (org.apache.lucene.analysis.en KStemFilter)
           (org.apache.lucene.util Version)
           (org.apache.lucene.analysis.tokenattributes CharTermAttribute)
           (org.apache.lucene.analysis Token)))

"Code some code borrowed from eandrejko/clj-tokenizer"

(defn- analyzed-token-stream
  "builds a TokenStream from provided string str with stopwords removed"
  [str]
  (let [sr (StringReader. str)
        sa (StandardAnalyzer. (Version/LUCENE_40))]
    (.tokenStream sa nil sr)))

(defn- stem-filter 
  [tk]
  (KStemFilter. tk))

(defn- next-token
  "reads the next token as a string from the TokenStream tk"
  [tk]
  (if (.incrementToken tk)
    (.toString (. tk getAttribute CharTermAttribute))))

(defn- token-seq
  "returns a sequence of tokens from the TokenStream tk"
  [tk]
  (lazy-seq
    (if-let [ntok (next-token tk)]
      (cons ntok (token-seq tk)))))

(defn only-words 
  "Removes all tokens containing only special characters or those not starting with a letter"
  [tokens]
  (filter #(re-matches #"([a-z]+)(.*)" %) tokens))

(defn indexed-token-map [word-bags] 
  "Returns a hashmap with the words as keys and their relative index (sorted) as value"
  (let [bag (ConcurrentSkipListSet.)]
    (do (doall (map #(. bag addAll %) word-bags))
      (into {} (map-indexed (fn [idx itm] [itm idx]) (.toArray bag))))))

(defn tokenize 
  "Tokenizes a string using the Lucene StandardAnalyzer and StandardTokenizer
  Lowercases string and removes words containing only non-alpha characters
  Returns a set" 
  [string]
  (set (token-seq (stem-filter (analyzed-token-stream (strs/lower-case string))))))
