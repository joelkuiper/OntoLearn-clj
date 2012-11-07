(ns indications.preprocess.tokenize
  (:require [clojure.string :as strs])
  (:import (org.apache.lucene.analysis.standard StandardTokenizer 
                                                StandardAnalyzer)
           java.io.StringReader
           (org.apache.lucene.analysis.en KStemFilter)
           (org.apache.lucene.util Version)
           ( org.apache.lucene.analysis.tokenattributes CharTermAttribute)
           (org.apache.lucene.analysis Token)))

"Code heavily borrowed from eandrejko/clj-tokenizer"

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
  "returns a lazy sequence of tokens from the TokenStream tk"
  [tk]
  (lazy-seq
   (if-let [ntok (next-token tk)]
     (cons ntok (token-seq tk)))))

(defn tokenize 
  "Tokenizes a string using the Lucene StandardAnalyzer and StandardTokenizer
  Lowercases string and removes words containing only non-alpha characters" 
  [string]
  (token-seq (stem-filter (analyzed-token-stream (strs/lower-case string)))))
