#
# Makefile to build erland module in 2 ways
#
# - traditionnal way in ebin/
# - with export all to do unit test in ebin-test/
#
ERL=/usr/bin/erl
ERLC=/usr/bin/erlc

all: ebin/geoserver.beam ebin/slippymap.beam ebin/tms.beam

dial: ebin-debug/geoserver.dial ebin-debug/slippymap.dial ebin-debug/tms.dial

test: ebin-test/test_all.beam ebin-test/geoserver.beam ebin-test/tms.beam ebin-test/slippymap.beam ebin-test/geoserver_tests.beam ebin-test/tms_tests.beam ebin-test/slippymap_tests.beam

dotest: test
	$(ERL) -noshell -pa ./ebin-test -s eunit test test_all -s init stop

clean:
	rm -fr ebin/*.beam
	rm -fr ebin-test/*.beam
	rm -fr ebin-debug/*.dial
	rm -fr ebin-debug/*.beam

ebin/%.beam: %.erl
	erlc -o ebin/ $<

ebin-test/%.beam: %.erl
	erlc -pa ./ebin-test -o ebin-test/ -W0 +export_all $<

ebin-debug/%.beam: %.erl
	erlc -pa ./ebin-debug -o ebin-debug/ -W0 +debug_info $<

ebin-debug/%.dial: ebin-debug/%.beam
	dialyzer --build_plt -pa ./ebin-debug -o $@ $<
