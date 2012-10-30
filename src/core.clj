(ns ml-indications.pubmed.retrieve
  (:import (uk.ac.ebi.ontocat OntologyServiceException
                              OntologyTerm
                              OntologyService$SearchOptions
                              file.FileOntologyService)
            java.net.URLEncoder)
)

(def human-do 
  (new FileOntologyService 
       (new java.net.URI 
            (.toString (io/as-url (io/resource "resources/HumanDO.obo"))))
  )
)

(defn search [terms]
  (map (fn [term] 
      (. human-do searchAll term (make-array OntologyService$SearchOptions 0))) terms)
)
