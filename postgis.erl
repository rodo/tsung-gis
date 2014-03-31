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
-export([rnd_box2d/0, r_box2d/1]).
-export([ewkb/1]).
-export([ewkb_point/1]).
-export([ewkb_point/2]).

-define(SRID, 4326).
-define(INDIAN, "00").

%% Tsung exports

r_point({_Pid,_DynVars})-> rnd_point().
r_point_srid({_Pid,_DynVars})-> rnd_point_srid().
r_box2d({_Pid,_DynVars})-> rnd_box2d().
ewkb_point({_Pid,_DynVars})-> ewkb_point().

%% Functions

rnd_box2d()->
    {FLat,FLon} = randomcoord:rcoord(),
    {SLat,SLon} = {randomcoord:rcoord(FLat, 90), randomcoord:rcoord(FLon, 180)},
    setsrid(lists:flatten(io_lib:format("ST_MakeBox2d(ST_Point(~.6f, ~.6f), ST_Point(~.6f, ~.6f))",
                                        [FLon, FLat, SLon, SLat]))).

rnd_point()->
    {Lat,Lon} = randomcoord:rcoord(),
    lists:flatten(io_lib:format("ST_Point(~.6f, ~.6f)",[Lon, Lat])).

rnd_point_srid()->
    {Lat,Lon} = randomcoord:rcoord(),
    setsrid(lists:flatten(io_lib:format("ST_Point(~.6f, ~.6f)",[Lon, Lat]))).

setsrid(Desc)->
    lists:flatten("ST_SetSRID("++ Desc ++"), "++io_lib:format("~p", [?SRID])++")").

ewkb(Value)->    
    Int = round(Value*100),
    string:left(integer_to_list(Int,16), 16, $0).

ewkb_point()->
    {Lat, Lon} = randomcoord:rcoord(),
    geom_prefix(point, {ewkb(Lat), ewkb(Lon)}).

ewkb_point(Lat, Lon)->
    geom_prefix(point, {ewkb(Lat), ewkb(Lon)}).

geom_prefix(point, {Lat, Lon})->
    "ST_GeomFromEWKB('"++lists:flatten([92,120])++?INDIAN++"00000001"++Lon++Lat++"'::bytea)".

  
