tsung-gis
=========

Tsung module for load test in GIS systems

Modules
=======

* randomcoord : generate random coordinate

* slippymap : convert coord to/from slippy map numbers quad tile

* tms : generate urls based on moves on a tile map

* wms : Web Mapping Service url generator

Running tests
=============

To run unit all tests you can do :

$ make dotest

Or for only one module, ``tms`` for example

$ make test
$ /usr/bin/erl -noshell -pa ./ebin-test -s eunit test tms_tests -s init stop
