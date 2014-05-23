tsung-gis
=========

Tsung module for load test in GIS systems

Templates
=========

build_scenario.py create a file called 'tsung.xml' in outputdir, and
a set of file load as ENTITY in 'tsung.xml' for each action defined in
all modules passed as parameter.

Usage :

$ build_scenario.py -o OUTPUT_DIR -m MODULE name

Sample usage :

$ build_scenario.py -o ~/tsung/bench_one/ -m wms -m xyz -m wfs

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

Documentation
=============

The documentation is online at http://tsung-gis.readthedocs.org/
