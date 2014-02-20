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
    ?assertEqual({-135.0,79.17133464081945}, slippymap:num2deg(4,4,5)).

num2deg2_test()->
    ?assertEqual({-180.0, 85.05112877980659}, slippymap:num2deg(0,0,5)).

deg2num_test()->
    ?assertEqual({13835,14066}, slippymap:deg2num(- 78, 124, 14)).

deg2num2_test()->
    ?assertEqual({13854,14271}, slippymap:deg2num(- 78.90 , 124.4 , 14)).

purge_test()->
    % list_url contains urls with layer name
    Urls = ["4/4/4","4/4/4","3/3/3"],
    Attend = ["3/3/3","4/4/4"],
    ?assertEqual(Attend, slippymap:purge(Urls)).

tmstowsm_test()->
    Dynvar = ts_dynvars:new([list_url],[["4/4/4/","5/5/5"]]),
    Attend = [{-123.75,76.84081641443098},{-90.0,66.51326044311186}],
    ?assertEqual(Attend, slippymap:tmstowms({4, Dynvar})).

tmstowsm_layer_test()->
    % list_url contains urls with layer name
    Dynvar = ts_dynvars:new([list_url],[["foo/4/4/4/","bar/5/5/5"]]),
    Attend = [{-123.75,76.84081641443098},{-90.0,66.51326044311186}],
    ?assertEqual(Attend, slippymap:tmstowms({4, Dynvar})).

tmstowsm_layerdup_test()->
    % list_url contains urls with layer name
    Dynvar = ts_dynvars:new([list_url],[["foo/4/4/4/","bar/4/4/4"]]),
    Attend = [{-90.0,66.51326044311186}],
    ?assertEqual(Attend, slippymap:tmstowms({4, Dynvar})).
