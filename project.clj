(defproject indications "0.1-SNAPSHOT"
  :description "Ontology based machine learning of indications in Randomized Controlled Trials"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [clj-http "0.5.5"]
                 [clj-xpath  "1.3.0"]
                 [org.clojure/tools.cli "0.2.2"]
                 [org.clojure/data.xml "0.0.6"]
                 [uk.ac.ebi/ontoCAT "0.9.9.3-DRUGIS"]
                 [com.taoensso/carmine "0.11.2"]
                 [org.clojure/data.csv "0.1.2"]
                 [org.apache.lucene/lucene-core "4.0.0-BETA"]
                 [org.apache.lucene/lucene-analyzers-common "4.0.0-BETA"]]
  :repositories {"drugis.org" "http://drugis.org/mvn/"
                 "project" "file:repo"
                 "java maven" "http://repo2.maven.org/maven2/"}
  :main         indications.core
  :jvm-ops      ["-Xmx2g" "--server"])

