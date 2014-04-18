%%
%% Copyright (c) 2014 Rodolphe Quiédeville <rodolphe@quiedeville.org>
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
%% PostgGIS functions
%%
-module(postgis).
-export([rnd_point/0, r_point/1, r_point_srid/1]).
-export([rnd_box2d/0, r_box2d/1, r_box2d_srid/1]).
-export([float_to_wkb/1]).
-export([wkb_point/1]).
-export([wkb_point/2]).


-define(SRID, 4326).
-define(INDIAN, "00").

%% Tsung exports

r_point({_Pid,_DynVars})-> rnd_point().
r_point_srid({_Pid,_DynVars})-> rnd_point_srid().
r_box2d({_Pid,_DynVars})-> rnd_box2d().
r_box2d_srid({_Pid,_DynVars})-> rnd_box2d_srid().
wkb_point({_Pid,_DynVars})-> wkb_point().

%% Functions

%%
%% 2 dimensions box with random coordinate
%%
rnd_box2d()->
    {FLat,FLon} = randomcoord:rcoord(),
    {SLat,SLon} = {randomcoord:rcoord(FLat, 90), randomcoord:rcoord(FLon, 180)},
    lists:flatten(io_lib:format("ST_MakeBox2d(ST_Point(~.6f, ~.6f), ST_Point(~.6f, ~.6f))",
                                        [FLon, FLat, SLon, SLat])).
%%
%% 2 dimensions box with random coordinate
%%
rnd_box2d_srid()->
    {FLat,FLon} = randomcoord:rcoord(),
    {SLat,SLon} = {randomcoord:rcoord(FLat, 90), randomcoord:rcoord(FLon, 180)},
    setsrid(lists:flatten(io_lib:format("ST_MakeBox2d(ST_Point(~.6f, ~.6f), ST_Point(~.6f, ~.6f))",
                                        [FLon, FLat, SLon, SLat]))).
%%
%% Point with random coordinate
%%
rnd_point()->
    {Lat,Lon} = randomcoord:rcoord(),
    lists:flatten(io_lib:format("ST_Point(~.6f, ~.6f)",[Lon, Lat])).

%%
%% Point with random coordinate, with SRID defined
%%
rnd_point_srid()->
    {Lat,Lon} = randomcoord:rcoord(),
    setsrid(lists:flatten(io_lib:format("ST_Point(~.6f, ~.6f)",[Lon, Lat]))).

%%
%% Return an object with SRID defined
%%
setsrid(Desc)->
    lists:flatten("ST_SetSRID("++ Desc ++", "++io_lib:format("~p", [?SRID])++")").

%% Convert a float to binary/64
%%
float_to_wkb(Value)->
    Bin = <<Value:64/float>>,
    bin_to_hex_list(Bin).

wkb_point()->
    {Lat, Lon} = randomcoord:rcoord(),
    wkb_geometry(point, {Lat, Lon}).

wkb_point(Lat, Lon)->
    wkb_geometry(point, {Lat, Lon}).

wkb_geometry(point, {Lat, Lon})->
    BLat = float_to_wkb(Lat),
    BLon = float_to_wkb(Lon),
    Prefix = wkb_prefix(point),
    ?INDIAN++Prefix++BLon++BLat.

bin_to_hex_list(Bin) when is_binary(Bin) ->
    Hex = lists:flatten([integer_to_list(X,16) || <<X>> <= Bin]),
    string:left(Hex, 16, $0).

wkb_prefix(Geometry)->
    case Geometry of
	point -> "00000001";
	linestring-> "00000002";
	polygon-> "00000003";
	multipoint-> "00000004";
	multilinestring->  "00000005";
	multipolygon-> "00000006"
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
setsrid_test()->
    Attend = "ST_SetSRID(Foo, 4326)",
    Result = setsrid("Foo"),
    ?assertEqual(Attend, Result).
-endif.
