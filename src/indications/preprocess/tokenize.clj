(ns indications.preprocess.tokenize
  (:require [clojure.string :as strs])
  (:import (org.apache.lucene.analysis.standard StandardTokenizer 
                                                StandardAnalyzer)
           java.io.StringReader
           java.util.concurrent.ConcurrentSkipListMap
           java.util.Arrays
           (org.apache.lucene.analysis.en KStemFilter)
           (org.apache.lucene.util Version)
           (org.apache.lucene.analysis.tokenattributes CharTermAttribute)
           (org.apache.lucene.analysis Token)))

"Some code borrowed from eandrejko/clj-tokenizer"

(defn- analyzed-token-stream
  "Builds a TokenStream from provided string str with stopwords removed"
  [str]
  (let [sr (StringReader. str)
        sa (StandardAnalyzer. (Version/LUCENE_40))]
    (.tokenStream sa nil sr)))

(defn- stem-filter 
  "See 'Viewing Morphology as an Inference Process' 
  (Krovetz, R., Proceedings 
  of the Sixteenth Annual International ACM SIGIR Conference on 
  Research and Development in Information Retrieval, 191-203, 1993)."
  [tk]
  (KStemFilter. tk))

(defn- next-token
  "Reads the next token as a string from the TokenStream tk"
  [tk]
  (if (.incrementToken tk)
    (.toString (. tk getAttribute CharTermAttribute))))

(defn- token-seq
  "Returns a sequence of tokens from the TokenStream tk"
  [tk]
  (lazy-seq
    (if-let [ntok (next-token tk)]
      (cons ntok (token-seq tk)))))

(defn only-words 
  "Removes all tokens containing only special characters or those not starting with a letter"
  [tokens]
  (filter #(re-matches #"([a-z]+)(.*)" %) tokens))

(defn map-count [map key] 
  (assoc map key (inc (get map key 0))))

(defn token-count [tokens] 
  (reduce map-count (sorted-map) tokens))

(defn tokenize 
  "Tokenizes a string using the Lucene StandardAnalyzer and StandardTokenizer
  Lowercases string and removes words containing only non-alpha characters
  Returns a set" 
  [string]
  (token-seq (stem-filter (analyzed-token-stream (strs/lower-case string)))))
