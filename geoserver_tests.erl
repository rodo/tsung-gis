%% -*- coding: utf-8 -*-  pylint: disable-msg=R0801
%%
%% Copyright (c) 2013 Rodolphe Quiédeville <rodolphe@quiedeville.org>
%%
%%     This program is free software: you can redistribute it and/or modify
%%     it under the terms of the GNU General Public License as published by
%%     the Free Software Foundation, either version 3 of the License, or
%%     (at your option) any later version.
%%
%%     This program is distributed in the hope that it will be useful,
%%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%     GNU General Public License for more details.
%%
%%     You should have received a copy of the GNU General Public License
%%     along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
-module(geoserver_tests).
-include_lib("eunit/include/eunit.hrl").
-author({author, "Rodolphe Quiédeville", "<rodolphe@quiedeville.org>"}).

%% Units tests
%% options/1
options_test() ->
    ?assertEqual("WIDTH=256", geoserver:default_option(width)).

options2_test() ->
    ?assertEqual("HEIGHT=256", geoserver:default_option(height)).

options4_test() ->
    ?assert(is_list(geoserver:default_option(height)) =:= true).

%% options/2
options3_test() ->
    ?assertEqual("HEIGHT=44", geoserver:options(height, ts_dynvars:new([height,width], [<<"44">>,<<"42">>]))).

%% buildurl/2
buildurl_test() ->
    ?assertEqual("HEIGHT=44&WIDTH=42&", geoserver:buildurl(ts_dynvars:new([height,width], [<<"44">>,<<"42">>]),[height,width])).

%% urlwms/1
urlwms_test()->
    Assert = "FORMAT=image%2Fpng&STYLES=&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&TILED=true&WIDTH=256&HEIGHT=256&TRANSPARENT=true&SRS=EPSG%3A900913&LAYERS=&TILESORIGIN=&BBOX=",
    ?assertEqual(Assert, geoserver:urlwms({2, ts_dynvars:new([foo], [<<"42">>])})).

%% urlwms/1
urlwms2_test()->
    Assert = "FORMAT=image%2Fpng&STYLES=&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&TILED=true&WIDTH=42&HEIGHT=256&TRANSPARENT=true&SRS=EPSG%3A900913&LAYERS=&TILESORIGIN=&BBOX=",
    ?assertEqual(Assert, geoserver:urlwms({2, ts_dynvars:new([width], [<<"42">>])})).

%% proj4erl
proj_test()->
    {ok, WGS84} = proj4:init("+init=epsg:4326"),
    {ok, CRS2180} = proj4:init("+init=epsg:2180"),
    P = {21.049804687501, 52.22900390625},
    {ok, P2} = proj4:transform(WGS84, CRS2180, P),
    ?assertEqual({639951.5695094677, 486751.7840663176}, P2).

