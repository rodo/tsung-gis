#
# Makefile to build erland module in 2 ways
#
# - traditionnal way in ebin/
# - with export all to do unit test in ebin-test/
#
ERL=/usr/bin/erl
ERLC=/usr/bin/erlc

all: ebin/map.beam ebin/wms.beam ebin/slippymap.beam ebin/tms.beam ebin/randomcoord.beam ebin/postgis.beam ebin/wkb.beam

dial: ebin-debug/map.dial ebin-debug/wms.dial ebin-debug/slippymap.dial ebin-debug/tms.dial ebin-debug/randomcoord.dial ebin-debug/postgis.dial ebin-debug/wkb.dial

test: ebin-test/test_all.beam ebin-test/map.beam ebin-test/wms.beam ebin-test/tms.beam ebin-test/slippymap.beam ebin-test/wms_tests.beam ebin-test/map_tests.beam ebin-test/tms_tests.beam ebin-test/slippymap_tests.beam ebin-test/randomcoord.beam ebin-test/randomcoord_tests.beam ebin-test/postgis.beam ebin-test/postgis_tests.beam  ebin-test/wkb.beam ebin-test/wkb_tests.beam

dotest: test
	$(ERL) -noshell -pa ./ebin-test -s eunit test test_all -s init stop

clean:
	rm -f *.beam
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
