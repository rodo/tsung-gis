tsung-gis
=========

Tsung module for load test in GIS systems

Running tests
=============

To run unit all tests you can do :

$ make dotest

Or for only one module, ``tms`` for example

$ make test
$ /usr/bin/erl -noshell -pa ./ebin-test -s eunit test tms_tests -s init stop

tms
===

Public methods you can use in your scenario

* move_first
* move_next
* move_right
* move_left
