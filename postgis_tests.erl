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

-module(postgis_tests).
-include_lib("eunit/include/eunit.hrl").
-author({author, "Rodolphe Quiédeville", "<rodolphe@quiedeville.org>"}).

r_point_test()->
    %%
    Url = postgis:r_point(),
    ?assertEqual(true, is_list(Url)).

r_point_tsung_test()->
    % Tsung call
    Pid = list_to_pid("<0.42.0>"),
    Url = postgis:r_point({Pid, []}),
    ?assertEqual(true, is_list(Url)).

r_box2d_test()->
    Url = postgis:r_box2d(),
    ?assertEqual(true, is_list(Url)).

r_box2d_tsung_test()->
    % Tsung call
    Pid = list_to_pid("<0.42.0>"),
    Url = postgis:r_box2d({Pid,[]}),
    ?assertEqual(true, is_list(Url)).

setsrid_test()->
    Attend = "ST_SetSRID(Foo), 4326)",
    Result = postgis:setsrid("Foo"),
    ?assertEqual(Attend, Result).
