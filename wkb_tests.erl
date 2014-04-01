%% -*- coding: utf-8 -*-
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
%% Unit tests for module : postgis

-module(wkb_tests).
-include_lib("eunit/include/eunit.hrl").
-author({author, "Rodolphe Quiédeville", "<rodolphe@quiedeville.org>"}).

% Point in Well Known Binary format
wkb_point_test()->
    Result = wkb:wkb_point(2.0, 4.0),
    Attend = "000000000140100000000000004000000000000000",
    ?assertEqual(Attend, Result).

% Convert binary to list in hex format
bin_to_hex_list_test()->
    Result = wkb:bin_to_hex_list(<<2.0:64/float>>),
    Attend = "4000000000000000",
    ?assertEqual(Attend, Result).

% Convert to Well Known Text value
float_to_wkb_test()->
    Result = wkb:float_to_wkb(4.0),
    Attend = "4010000000000000",
    ?assertEqual(Attend, Result).

% Point in Well Known Binary format
wkb_linestring_test()->
    Result = wkb:wkb_linestring([{2.0, 4.0}, {42.5, -1.25}]),
    Attend = "00000000020000000240100000000000004000000000000000BFF40000000000004045400000000000",
    ?assertEqual(Attend, Result).

% Point
wkb_geometry_point_test()->
    Result = wkb:wkb_geometry(point, {72.321654, -54.654654654}),
    Attend = "0000000001C04B53CBB9448D5640521495FAA8A82A",
    ?assertEqual(Attend, Result).

% Linestring
wkb_geometry_linestring_test()->
    Result = wkb:wkb_geometry(linestring, [{2.0, 4.0}, {42.5, -1.25}]),
    Attend = "00000000020000000240100000000000004000000000000000BFF40000000000004045400000000000",
    ?assertEqual(Attend, Result).

% 
wkb_geom_test()->
    Result = wkb:geom(point, {2.0, 4.0}),
    Attend = {"00000001","40100000000000004000000000000000"},
    ?assertEqual(Attend, Result).


% 
wkb_geoms_empty_test()->
    Result = wkb:geoms(linestring, [], {0, "Foo"}),
    Attend = {0, "Foo"},    
    ?assertEqual(Attend, Result).

wkb_geoms_test()->
    Result = wkb:geoms(linestring, [{42.563, -1.2}, {66.3652, 11.2}], {0, ""}),
    Attend = {2, "BFF333333333333340454810624DD2F240266666666666664050975F6FD21FF3"},    
    ?assertEqual(Attend, Result).
