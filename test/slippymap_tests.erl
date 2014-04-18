%
% @doc OpenStreetMap slippy map tile numbers
%
% @author Rodolphe Quiedeville <rodolphe@quiedeville.org>
%   [http://rodolphe.quiedeville.org]
%
% @copyright 2013 Rodolphe Quiedeville
%
% @reference <a href="http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames">http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames</a>

-module(slippymap_tests).
-include_lib("eunit/include/eunit.hrl").

num2deg_test()->
    ?assertEqual({-135.0,79.17133464}, slippymap:num2deg(4,4,5)).

num2deg2_test()->
    ?assertEqual({-180.0, 85.05112878}, slippymap:num2deg(0,0,5)).

num2deg_zoom15_test()->
    ?assertEqual({-2.97729492,47.70976154}, slippymap:num2deg(16113,11430,15)).

deg2num_test()->
    ?assertEqual({13835,14066}, slippymap:deg2num(- 78, 124, 14)).

deg2num2_test()->
    ?assertEqual({13854,14271}, slippymap:deg2num(- 78.90 , 124.4 , 14)).

tmstowsm_test()->
    Dynvar = ts_dynvars:new([list_url],[["4/4/4/","5/5/5"]]),
    Attend = [{-123.75,74.01954331,-112.5,76.84081641},
              {-90.0,55.77657302,-67.5, 66.51326044}],
    ?assertEqual(Attend, slippymap:tmstowms({4, Dynvar})).

tmstowsm_layer_test()->
    % list_url contains urls with layer name
    Dynvar = ts_dynvars:new([list_url],[["foo/4/4/4/","bar/5/5/5"]]),
    Attend = [{-123.75,74.01954331,-112.5,76.84081641},
              {-90.0,55.77657302,-67.5,66.51326044}],
    ?assertEqual(Attend, slippymap:tmstowms({4, Dynvar})).

tmstowsm_layerdup_test()->
    % list_url contains urls with layer name
    Dynvar = ts_dynvars:new([list_url],[["foo/4/4/4/","bar/4/4/4"]]),
    Attend = [{-90.0,55.77657302,-67.5,66.51326044}],
    ?assertEqual(Attend, slippymap:tmstowms({4, Dynvar})).

tile2lat_test()->
    ?assertEqual(83.99996604, slippymap:tile2lat(503,14)).

tile2lon_test()->
    ?assertEqual(-23.95019531, slippymap:tile2lon(3551,13)).

num2bbox_test()->
    Attend = {-178.29025269,84.38112074,-178.28887939,84.38125519},
    ?assertEqual(Attend, slippymap:num2bbox(1245, 5304, 18)).
