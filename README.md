postatic
========

A generator for static websites made up of article-like postings.

[![Build Status](https://travis-ci.org/friemen/postatic.png?branch=master)](https://travis-ci.org/friemen/postatic)

Usage
-----

To build and start postatic, clone this repo, cd into the project directory and type

    lein uberjar

I added some sample articles, a stylesheet and a template in the folder sample-data.
The setup in sample-data is ready for testing the production process.

    cd sample-data
    ./produce.sh

You'll find the resulting files in sample-data/output. Open the index.html to
see the generated HTML output.
    
To make your own static article website copy the sample-data folder,
add the target/postatic*-standalone.jar to your copy and adapt the shell
script produce.sh to fit your needs.


License
=======

Copyright 2013 F.Riemenschneider

Distributed under the Eclipse Public License, the same as Clojure.
