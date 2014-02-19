%% -*- coding: utf-8 -*-
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
%%  url to include in tsung config file
%%
%%  Randomize url in tile server form used by openstreetmap project
%%
%%   - urlzxy : return a single url
%%
%%  <request subst="true">
%%    <http url='/%%wmsosm:urlzxy%%.png' version='1.1' method='GET'></http>
%%  </request>

-module(wmsosm_tests).
-include_lib("eunit/include/eunit.hrl").
-author({author, "Rodolphe Quiédeville", "<rodolphe@quiedeville.org>"}).

zoomlevel_test()->
    ?assertEqual("3/",wmsosm:zoomlevel(3)).

elmts_test() ->
    ?assertEqual(171, wmsosm:elmts(18)).

fillall_test()->
    ?assertEqual([16,17,17,18,18,18],wmsosm:fillall(16,[])).

get_square_size_test()->
    ?assertEqual([3,3], wmsosm:get_square_size(ts_dynvars:new([map_width,map_height], [<<"512">>,<<"512">>]))).

get_square_size_min_test()->
    ?assertEqual([1,3], wmsosm:get_square_size(ts_dynvars:new([map_width], [<<"0">>]))).

get_square_size_max_test()->
    ?assertEqual([4,8], wmsosm:get_square_size(ts_dynvars:new([map_height], [<<"256000">>]))).

get_square_size_list_test()->
    ?assertEqual([4,4], wmsosm:get_square_size(ts_dynvars:new([map_height], ["768"]))).

get_square_size_int_test()->
    ?assertEqual([4,4], wmsosm:get_square_size(ts_dynvars:new([map_height], [768]))).

read_ssize_w_zero_test()->
    %% The size is defined
    ?assertEqual(1, wmsosm:read_ssize(ts_dynvars:new([map_width], [<<"0">>]), width)).

read_ssize_w_exists_test()->
    %% The size is defined
    ?assertEqual(3, wmsosm:read_ssize(ts_dynvars:new([map_width], [<<"512">>]), width)).

read_ssize_w_not_exists_test()->
    %% The size is defined
    ?assertEqual(4, wmsosm:read_ssize(ts_dynvars:new(), width)).

read_ssize_h_exists_test()->
    %% The height is defined in DynVars
    DynVars = ts_dynvars:new([map_width, map_height], [<<"400">>,<<"768">>]),
    ?assertEqual(4, wmsosm:read_ssize(DynVars, height)).

read_ssize_h_not_exists_test()->
    %% The size is defined
    ?assertEqual(3, wmsosm:read_ssize(ts_dynvars:new(), height)).

read_ssize_notexists_test()->
    %% The size is defined
    ?assertEqual(3, wmsosm:read_ssize(ts_dynvars:new([foobar], [<<"8">>]), height)).

get_urlblock_test()->
    %% The size is defined
    Urls = wmsosm:get_urlblock({42, ts_dynvars:new([map_height], [<<"800">>])}),
    ?assertEqual(16, length(Urls)).

get_urlblock_empty_test()->
    %% The size is not defined
    Urls = wmsosm:get_urlblock({42, ts_dynvars:new()}),
    ?assertEqual(12, length(Urls)).

zxy_test()->
    ?assertEqual(3, length(wmsosm:zxy())).

url_split_test()->
    ?assertEqual([12, 1242, 3345], wmsosm:url_split("12/1242/3345")).

random_action1_test()->
    ?assertEqual(move,  wmsosm:random_action(3)).

random_action2_test()->
    ?assertEqual(zoom,  wmsosm:random_action(53)).

random_move1_test()->
    Actions = [{top, 1}, {bottom, 2}, {left, 3}, {right, 4}],
    ?assertNot(false =:= lists:keyfind(wmsosm:random_move(), 1, Actions)).

binary_to_number_test()->
    ?assertEqual(5.6, wmsosm:binary_to_number(<<"5.6">>)).

list_to_number_test()->
    ?assertEqual(5.6, wmsosm:list_to_number("5.6")).

list_to_number_int_test()->
    ?assertEqual(5, wmsosm:list_to_number("5")).

list_to_number_int2_test()->
    ?assertEqual(-15, wmsosm:list_to_number("-15")).

%% up to one zoom level
zoom_move_more_test()->
    ?assertEqual(["13/24/18"], wmsosm:zoom_move("12/12/9", [1,1], more)).

%% stay on the same zoom level, limit max reached

zoom_move_less_test()->
    ?assertEqual(["11/13/15"], wmsosm:zoom_move("12/26/30", [1,1], less)).

new_zoom_more_test()->
    ?assertEqual(4, wmsosm:new_zoom(3, more)).

new_zoom_less_test()->
    ?assertEqual(4, wmsosm:new_zoom(5, less)).

new_zoom_more_limit_test()->
    ?assert(18 > wmsosm:new_zoom(18, more)).
%%
%%
%%
last_block_defined_test()->
    ?assertEqual(["8/9/10"], wmsosm:last_block(ts_dynvars:new([list_url], [["8/9/10"]]))).

last_block_undefined_test()->
    ?assertEqual(12, length(wmsosm:last_block(ts_dynvars:new()))).
%%
%% First move
%%
move_first_undefined_test()->
    %% the startpoint is undefined
    ?assertEqual(12, length(wmsosm:move_first({4, ts_dynvars:new()}))).

move_first_defined_test()->
    %% the startpoint is defined
    Assert = ["2/1/1","2/1/2","2/1/3","2/2/1","2/2/2","2/2/3","2/3/1","2/3/2","2/3/3","2/4/1","2/4/2","2/4/3"],
    ?assertEqual(Assert, wmsosm:move_first({4, ts_dynvars:new([first_url],["2/1/1"])})).
%%
%%
%%
coord_zoom_up_test()->
    ?assertEqual(8, wmsosm:coord_zoom(4, 10, 11)).

coord_zoom_2up_test()->
    ?assertEqual(16, wmsosm:coord_zoom(4, 10, 12)).

coord_zoom_1down_test()->
    ?assertEqual(130, wmsosm:coord_zoom(259, 9, 8)).

%%
%%
%%
url_corner_tl_test()->
    Assert = ["2/1/1","2/1/2","2/1/3","2/2/1","2/2/2","2/2/3","2/3/1","2/3/2","2/3/3","2/4/1","2/4/2","2/4/3"],
    ?assertEqual("2/1/1", wmsosm:url_corner(Assert, top_left)).

url_corner_br_test()->
    Assert = ["2/1/1","2/1/2","2/1/3","2/2/1","2/2/2","2/2/3","2/3/1","2/3/2","2/3/3","2/4/1","2/4/2","2/4/3"],
    ?assertEqual("2/4/3", wmsosm:url_corner(Assert, bottom_right)).
%%
%%
move_left_test()->
    %% Move 1 time to the left
    Assert = ["2/0/1","2/0/2","2/0/3","2/0/4"],
    ?assertEqual(Assert, wmsosm:move("2/1/1",[4,4], 1, left)).

move_right_test()->
    %% Move 1 time to the right, add 1 to X
    Assert = ["2/2/1","2/2/2","2/2/3","2/2/4"],
    ?assertEqual(Assert, wmsosm:move("2/1/1",[4,4], 1, right)).

move_bottom_test()->
    %% Move 1 time to the bottom, add 1 to Y
    Assert = ["4/5/5","4/6/5","4/7/5"],
    ?assertEqual(Assert, wmsosm:move("4/5/4",[3,6], 1, bottom)).

move_top_test()->
    %% Move 1 time to the top, sub 1 from Y
    Assert = ["6/1/0","6/2/0","6/3/0","6/4/0","6/5/0"],
    ?assertEqual(Assert, wmsosm:move("6/1/1",[5,4], 1, top)).

move_top2_test()->
    %% Move 2 time to the top, sub 2 from Y
    Assert = ["6/1/6","6/2/6","6/3/6","6/4/6","6/5/6"],
    ?assertEqual(Assert, wmsosm:move("6/1/8",[5,4], 2, top)).

move_top3_test()->
    %% Move 12 time to the top, sub 12 from Y
    %% move is over limit
    Assert = ["6/1/0","6/2/0","6/3/0","6/4/0","6/5/0"],
    ?assertEqual(Assert, wmsosm:move("6/1/8",[5,4], 12, top)).
