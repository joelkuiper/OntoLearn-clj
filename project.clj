(defproject indications "0.1-SNAPSHOT"
  :description "Ontology based machine learning of indications in Randomized Controlled Trials"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [clj-http "0.5.5"]
                 [clj-xpath  "1.3.0"]
                 [local/ontocat "0.9.9.2"]
                 [com.taoensso/carmine "0.11.2"]]
  :repositories {"drugis.org" "http://drugis.org/mvn/"
                 "project" "file:repo"
                 "java maven" "http://repo2.maven.org/maven2/"}
  :main         indications.core
  :jvm-ops      ["-Xmx8g" "-Xmn1g" "-server"])

