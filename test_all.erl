%%
%% Laod all unit tests to run them all at once
%%
-module(test_all).
-include_lib("eunit/include/eunit.hrl").

all_test_() -> [map_tests,
		postgis_tests,
                randomcoord_tests,
                slippymap_tests,
                tms_tests,
		wkb_tests,
                wms_tests].
