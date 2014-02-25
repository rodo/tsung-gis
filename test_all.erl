%%
%% Laod all unit tests to run them all at once
%%
-module(test_all).
-include_lib("eunit/include/eunit.hrl").

all_test_() -> [map_tests,
                wms_tests,
                slippymap_tests,
                randomcoord_tests,
                tms_tests].
