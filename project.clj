(defproject ml-indications "0.1-SNAPSHOT"
  :description "Ontology based machine learning of indications in Randomized Controlled Trials"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [clj-http "0.5.5"]
                 [clj-xpath  "1.3.0"]
                 [uk.ac.ebi/ontoCAT "0.9.9.3-DRUGIS"]
                 [com.taoensso/carmine "0.11.2"]]
  :repositories {"drugis.org" "http://drugis.org/mvn/"})

