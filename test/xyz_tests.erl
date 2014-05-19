%% -*- coding: utf-8 -*-  pylint: disable-msg=R0801
%%
%% Copyright (c) 2013,2014 Rodolphe Quiédeville <rodolphe@quiedeville.org>
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
-module(xyz_tests).
-include_lib("eunit/include/eunit.hrl").
-author({author, "Rodolphe Quiédeville", "<rodolphe@quiedeville.org>"}).

%% Units tests

pixel2tiles_test()->
    ?assertEqual(5, xyz:pixel2tiles(800, 256)).

pixel2tiles_null_test()->
    ?assertEqual(1, xyz:pixel2tiles(800, 0)).

readssize_height_test() ->
    Res = xyz:read_ssize(ts_dynvars:new([map_height], [<<"640">>]), height),
    ?assertEqual(4, Res).

readssize_width_test() ->
    Res = xyz:read_ssize(ts_dynvars:new([map_width], [<<"960">>]), width),
    ?assertEqual(5, Res).

% The height is not set
readssize_height_empty_test() ->
    Res = xyz:read_ssize(ts_dynvars:new([foo],[2]), height),
    ?assertEqual(4, Res).

% The width is not set
readssize_width_empty_test() ->
    Res = xyz:read_ssize(ts_dynvars:new([foo],[2]), width),
    ?assertEqual(5, Res).
