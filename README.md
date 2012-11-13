**This is work in process and definitely not something you'd want to use in
production**

# Clojure PubMed to LibSVM utility

Collection of utilities for retrieving and processing [PubMed](http://www.ncbi.nlm.nih.gov/pubmed/) files into to [LibSVM](http://www.csie.ntu.edu.tw/~cjlin/libsvm/) data format. 

Used for matching ontologies to MeSH annotated articles and inferring labels 
for those not annotated using Support Vector Machines with linear kernel.

Specifically used with [Human Disease Ontology](http://www.disease-ontology.org/)
with on a large (5 GB) collection Randomized Clinical Trials.

## Usage

* import/retrieve contains a wrapper for downloading XML Records based on a query
to disk.
* import/process contains utilities for writing fields in PubMed XML files to a [Redis](http://redis.io/)
  back-end by using XSLT. 
* pre-process includes stop-word filtering and stemming using Lucene on article
  abstracts
* core includes code to write to .libsvm files

## License
Copyright (c) 2012, Joel Kuiper
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the organization nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL JOEL KUIPER BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
