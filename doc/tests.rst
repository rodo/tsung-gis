.. _sec-tests:

Runnins tests
=============

To run unit all tests you can do :

$ make dotest

Or for only one module, ``tms`` for example

$ make test
$ /usr/bin/erl -noshell -pa ./ebin-test -s eunit test tms_tests -s init stop

Continuous integration
======================

A Jenkins job is set up for **tsung-gis** at
http://jenkins.quiedeville.org/view/Tsung/job/TsungGIS/

