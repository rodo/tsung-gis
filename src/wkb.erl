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
%% Well Known Binary conversion functions
%% http://en.wikipedia.org/wiki/Well-known_text#Well-known_binary
-module(wkb).
%% Exports
-export([float_to_wkb/1]).
-export([wkb_point/1]).
-export([wkb_point/2]).
-export([wkb_linestring/1]).
-export([geoms/3]).
-export([geoms/2]).
%
-ifdef(TEST).
-export([geom/2,wkb_geometry/2,bin_to_hex_list/1]).
-endif.
%% Tsung exports
wkb_point({_Pid,_DynVars})-> wkb_point().

-define(SRID, 4326).
-define(INDIAN, "00").


%% Functions

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

wkb_linestring(Coords)->
    wkb_geometry(linestring, Coords).

wkb_geometry(linestring, Coords)->
    ?INDIAN++geoms(linestring, Coords);
wkb_geometry(point, Coords)->
    {P, C} = geom(point, Coords),
    ?INDIAN++P++C.


geoms(Geom, Coords)->
    {Count, Coord} = geoms(Geom, Coords, {0, ""}),
    wkb_prefix(Geom)++string:right(lists:flatten(io_lib:format("~p", [Count])), 8, $0)++Coord.

geoms(_, [], {Count, Coords})->    
    {Count, Coords};
geoms(Geom, [H|T], {Count, Coords})->
    {_P, C} = geom(Geom, H),
    geoms(Geom, T, {Count + 1, Coords ++ C}).

geom(Geom, {Lat, Lon})->
    {wkb_prefix(Geom), float_to_wkb(Lon)++float_to_wkb(Lat)}.



bin_to_hex_list(Bin) when is_binary(Bin) ->
    Hex = lists:flatten([integer_to_list(X,16) || <<X>> <= Bin]),
    string:left(Hex, 16, $0).

wkb_prefix(Geometry)->
    case Geometry of
	point          -> "00000001";
	linestring     -> "00000002";
	polygon        -> "00000003";
	multipoint     -> "00000004";
	multilinestring-> "00000005";
	multipolygon   -> "00000006"
    end.
