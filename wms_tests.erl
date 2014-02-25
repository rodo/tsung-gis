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
-module(wms_tests).
-include_lib("eunit/include/eunit.hrl").
-author({author, "Rodolphe Quiédeville", "<rodolphe@quiedeville.org>"}).

%% Units tests
%% options/1
options_test() ->
    ?assertEqual("WIDTH=256", wms:default_option(width)).

options2_test() ->
    ?assertEqual("HEIGHT=256", wms:default_option(height)).

options4_test() ->
    ?assert(is_list(wms:default_option(height)) =:= true).

%% options/2
options3_test() ->
    ?assertEqual("HEIGHT=44", wms:options(height, ts_dynvars:new([height,width], [<<"44">>,<<"42">>]))).

%% buildurl/2
buildurl_test() ->
    ?assertEqual("HEIGHT=44&WIDTH=42&", wms:buildurl(ts_dynvars:new([height,width], [<<"44">>,<<"42">>]),[height,width])).

%% url/1
url_test()->
    Assert = "FORMAT=image%2Fpng&STYLES=&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&TILED=true&WIDTH=256&HEIGHT=256&TRANSPARENT=true&SRS=EPSG%3A900913&LAYERS=&TILESORIGIN=&BBOX=",
    ?assertEqual(Assert, wms:url({2, ts_dynvars:new([foo], [<<"42">>])})).

%% url/1
url2_test()->
    Assert = "FORMAT=image%2Fpng&STYLES=&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&TILED=true&WIDTH=42&HEIGHT=256&TRANSPARENT=true&SRS=EPSG%3A900913&LAYERS=&TILESORIGIN=&BBOX=",
    ?assertEqual(Assert, wms:url({2, ts_dynvars:new([width], [<<"42">>])})).

%% Layer is defined in dynvars
url_layer_test()->
    Dynvars = ts_dynvars:new([width,layers], [<<"42">>,<<"DataGouv">>]),
    Assert = "FORMAT=image%2Fpng&STYLES=&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&TILED=true&WIDTH=42&HEIGHT=256&TRANSPARENT=true&SRS=EPSG%3A900913&LAYERS=DataGouv&TILESORIGIN=&BBOX=",
    ?assertEqual(Assert, wms:url({2, Dynvars})).

%% BBOX defined in dynvars
url_bbox_test()->
    Dynvars = ts_dynvars:new([width,layers,bbox], [<<"42">>,<<"DataGouv">>,<<"0,1,2,3">>]),
    Assert = "FORMAT=image%2Fpng&STYLES=&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&TILED=true&WIDTH=42&HEIGHT=256&TRANSPARENT=true&SRS=EPSG%3A900913&LAYERS=DataGouv&TILESORIGIN=&BBOX=0%2C1%2C2%2C3",
    ?assertEqual(Assert, wms:url({2, Dynvars})).

%% Styles is defined in dynvars
url_style_test()->
    Dynvars = ts_dynvars:new([width,layers,bbox,styles], [<<"42">>,<<"DataGouv">>,<<"0,1,2,3">>,<<"blue">>]),
    Assert = "FORMAT=image%2Fpng&STYLES=blue&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&TILED=true&WIDTH=42&HEIGHT=256&TRANSPARENT=true&SRS=EPSG%3A900913&LAYERS=DataGouv&TILESORIGIN=&BBOX=0%2C1%2C2%2C3",
    ?assertEqual(Assert, wms:url({2, Dynvars})).

%% Format is defined in dynvars
url_format_test()->
    Dynvars = ts_dynvars:new([format], [<<"image/jpeg">>]),
    Assert = "FORMAT=image%2Fjpeg&STYLES=&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&TILED=true&WIDTH=256&HEIGHT=256&TRANSPARENT=true&SRS=EPSG%3A900913&LAYERS=&TILESORIGIN=&BBOX=",
    ?assertEqual(Assert, wms:url({2, Dynvars})).


move_first_test()->
    %% the startpoint is defined
    DynVars = ts_dynvars:new([first_url,list_url,map_width,map_height], ["2/1/1",["2/1/1"],<<"400">>,<<"400">>]),
    Assert = ["2/1/1","2/1/2","2/2/1","2/2/2"],
    ?assertEqual(Assert, wms:move_first({4, DynVars})).
