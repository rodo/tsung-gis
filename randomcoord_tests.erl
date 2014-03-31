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
%% Unit tests for module : randomcoord

-module(randomcoord_tests).
-include_lib("eunit/include/eunit.hrl").
-author({author, "Rodolphe Quiédeville", "<rodolphe@quiedeville.org>"}).

url_test()->
    Url = randomcoord:url({4,4}),
    ?assertEqual(true, is_list(Url)).

rcoord_test()->
    Rcoord = randomcoord:rcoord(),
    ?assertEqual(true, is_tuple(Rcoord)).

rcoord_values_test()->
    {Lat, Lon} = randomcoord:rcoord(),
    ?assertEqual(true, is_float(Lon)),
    ?assertEqual(true, is_float(Lat)).

rcoord_limit_test()->
    Rcoord = randomcoord:rcoord(45.0 ,50.0),
    ?assertEqual(true, Rcoord >= 45.0),
    ?assertEqual(true, Rcoord =< 50.0).

rcoord_bbox_lon_test()->
    {Lat, Lon} = randomcoord:rcoord(45.0,50.0,60.0,70.0),
    ?assertEqual(true, Lat >= 50.0),
    ?assertEqual(true, Lat =< 70.0),
    ?assertEqual(true, Lon >= 45.0),
    ?assertEqual(true, Lon =< 60.0).

longitude_test()->
    Lon = randomcoord:longitude(20.0,22.0),
    ?assertEqual(true, is_float(Lon)).

latitude_test()->
    Lat = randomcoord:latitude(40.0,42.0),
    ?assertEqual(true, is_float(Lat)).
