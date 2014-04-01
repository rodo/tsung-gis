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
-export([wkb_linestring/4]).
-export([geoms/3]).
-export([geoms/2]).
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
    wkb_geometry(point, [{Lat, Lon}]).

wkb_linestring(Lat, Lon, BLat, BLon)->
    wkb_geometry(linestring, [{Lat, Lon}, {BLat, BLon}]).

wkb_geometry(linestring, Coords)->
    ?INDIAN++geoms(linestring, Coords);
wkb_geometry(point, Coords)->
    [{Lat, Lon}] = Coords,
    BLat = float_to_wkb(Lat),
    BLon = float_to_wkb(Lon),
    Prefix = wkb_prefix(point),
    ?INDIAN++Prefix++BLon++BLat.


geoms(Geom, Coords)->
    {Pref, Coord} = geoms(Geom, Coords, {"", ""}),
    Pref++Coord.

geoms(Geom, [], {Prefixs, Coords})->
    Geom, [], {Prefixs, Coords};
geoms(Geom, [H|T], {Prefixs, Coords})->
    {P, C} = geom(Geom, H),
    geoms(Geom, T, {Prefixs ++ P, Coords ++ C}).


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


%%

%% 0102 00000000 000000000016400000000000404540  0000000000000000002F4000000000004045C0


%% 0102 00000002000000000000000000164000000000004045400000000000002F4000000000004045C0

%% 00000000024000000000000000401000000000000040160000000000004045400000000000

%% 000000000140000000000000004010000000000000000000000140160000000000004045400000000000

%% 0000000002.4000000000000000.4010000000000000.4016000000000000.4045400000000000



%%         0101000000
%%         0101000000 00000000000016400000000000404540
%%                                          0101000000 0000000000002F4000000000004045C0
%% 010200000002000000 00000000000016400000000000404540 0000000000002F4000000000004045C0
%% 00000000024000000000000000401000000000000040160000000000004045400000000000
