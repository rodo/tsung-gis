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
%% Unit tests for tms module

-module(tms_tests).
-include_lib("eunit/include/eunit.hrl").
-author({author, "Rodolphe Quiédeville", "<rodolphe@quiedeville.org>"}).

zoomlevel_test()->
    ?assertEqual("3/",tms:zoomlevel(3)).

elmts_test() ->
    ?assertEqual(171, tms:elmts(18)).

fillall_test()->
    ?assertEqual([16,17,17,18,18,18],tms:fillall(16,[])).

get_square_size_test()->
    ?assertEqual([3,3], tms:get_square_size(ts_dynvars:new([map_width,map_height], [<<"512">>,<<"512">>]))).

get_square_size_min_test()->
    ?assertEqual([1,3], tms:get_square_size(ts_dynvars:new([map_width], [<<"0">>]))).

get_square_size_max_test()->
    ?assertEqual([4,8], tms:get_square_size(ts_dynvars:new([map_height], [<<"256000">>]))).

get_square_size_list_test()->
    ?assertEqual([4,4], tms:get_square_size(ts_dynvars:new([map_height], ["768"]))).

get_square_size_int_test()->
    ?assertEqual([4,4], tms:get_square_size(ts_dynvars:new([map_height], [768]))).
%%
%% 
%%
read_ssize_w_zero_test()->
    %% size value is not conform, return the default min
    ?assertEqual(1, tms:read_ssize(ts_dynvars:new([map_width], [<<"0">>]), width)).

read_ssize_w_exists_test()->
    %% The size is defined
    ?assertEqual(3, tms:read_ssize(ts_dynvars:new([map_width], [<<"512">>]), width)).

read_ssize_w_not_exists_test()->
    %% The size is not defined, return default
    ?assertEqual(4, tms:read_ssize(ts_dynvars:new(), width)).

read_ssize_h_exists_test()->
    %% The width and height are defined in DynVars
    DynVars = ts_dynvars:new([map_width, map_height], [<<"400">>,<<"768">>]),
    ?assertEqual(4, tms:read_ssize(DynVars, height)).

read_ssize_h_not_exists_test()->
    %% The size is not defined in DynVars
    ?assertEqual(3, tms:read_ssize(ts_dynvars:new(), height)).

read_ssize_notexists_test()->
    %% The size is defined
    ?assertEqual(3, tms:read_ssize(ts_dynvars:new([foobar], [<<"8">>]), height)).
%%
%%
%%
get_urlblock_test()->
    %% The size is defined
    Urls = tms:get_urlblock({42, ts_dynvars:new([map_height], [<<"800">>])}),
    ?assertEqual(16, length(Urls)).

get_urlblock_empty_test()->
    %% The size is not defined
    Urls = tms:get_urlblock({42, ts_dynvars:new()}),
    ?assertEqual(12, length(Urls)).

layers_test()->
    %% Two layers are defined
    Layers = tms:layers(ts_dynvars:new([tms_layers], ["france,roads"])),
    ?assertEqual(Layers, ["france","roads"]).

layers_empty_test()->
    %% No layers are defined
    Layers = tms:layers(ts_dynvars:new()),
    ?assertEqual(Layers, []).

zxy_test()->
    ?assertEqual(3, length(tms:zxy())).
%%
%%
%%
url_split_test()->
    ?assertEqual([12, 1242, 3345], tms:url_split("12/1242/3345")).

url_split_layer_test()->
    ?assertEqual([12, 1242, 3345], tms:url_split("foobar/12/1242/3345")).

split_test()->
    ?assertEqual(["12","13","14"], tms:split("12/13/14")).

split_with_layer_test()->
    % Split an url that contains a layer inside
    ?assertEqual(["12","13","14"], tms:split("layer/12/13/14")).
%%
%%
random_action1_test()->
    ?assertEqual(move,  tms:random_action(3)).

random_action2_test()->
    ?assertEqual(zoom,  tms:random_action(93)).

random_move1_test()->
    Actions = [{top, 1}, {bottom, 2}, {left, 3}, {right, 4}],
    ?assertNot(false =:= lists:keyfind(tms:random_move(), 1, Actions)).

binary_to_number_test()->
    ?assertEqual(5.6, tms:binary_to_number(<<"5.6">>)).

list_to_number_test()->
    ?assertEqual(5.6, tms:list_to_number("5.6")).

list_to_number_int_test()->
    ?assertEqual(5, tms:list_to_number("5")).

list_to_number_int2_test()->
    ?assertEqual(-15, tms:list_to_number("-15")).

%% up to one zoom level
zoom_move_more_test()->
    ?assertEqual(["13/24/18"], tms:zoom_move("12/12/9", [1,1], more)).

%% stay on the same zoom level, limit max reached

zoom_move_less_test()->
    ?assertEqual(["11/13/15"], tms:zoom_move("12/26/30", [1,1], less)).

new_zoom_more_test()->
    ?assertEqual(4, tms:new_zoom(3, more)).

new_zoom_less_test()->
    ?assertEqual(4, tms:new_zoom(5, less)).

new_zoom_more_limit_test()->
    ?assert(18 > tms:new_zoom(18, more)).
%%
%%
%%
last_block_defined_test()->
    ?assertEqual(["8/9/10"], tms:last_block(ts_dynvars:new([list_url], [["8/9/10"]]))).

last_block_undefined_test()->
    ?assertEqual(12, length(tms:last_block(ts_dynvars:new()))).
%%
%% First move
%%
move_first_undefined_test()->
    %% the startpoint is undefined
    ?assertEqual(12, length(tms:move_first({4, ts_dynvars:new()}))).

move_first_defined_test()->
    %% the startpoint is defined
    DynVars = ts_dynvars:new([first_url,map_width,map_height],
                             ["2/1/1",<<"400">>,<<"400">>]),
    Assert = ["2/1/1","2/1/2","2/2/1","2/2/2"],
    ?assertEqual(Assert, tms:move_first({4, DynVars})).

move_first_layers_test()->
    %% the first move on 3 layers
    DynVars = ts_dynvars:new([first_url,map_width,map_height,tms_layers],
                             ["2/1/1",<<"400">>,<<"400">>,"a,b,c"]),
    Assert = ["a/2/1/1","a/2/1/2","a/2/2/1","a/2/2/2",
              "b/2/1/1","b/2/1/2","b/2/2/1","b/2/2/2",
              "c/2/1/1","c/2/1/2","c/2/2/1","c/2/2/2"
	     ],
    ?assertEqual(Assert, tms:move_first_layers({4, DynVars})).

%%
%%
%%
coord_zoom_up_test()->
    ?assertEqual(8, tms:coord_zoom(4, 10, 11)).

coord_zoom_2up_test()->
    ?assertEqual(16, tms:coord_zoom(4, 10, 12)).

coord_zoom_1down_test()->
    ?assertEqual(130, tms:coord_zoom(259, 9, 8)).

%%
%%
%%
url_corner_tl_test()->
    Assert = ["2/1/1","2/1/2","2/1/3","2/2/1","2/2/2","2/2/3","2/3/1","2/3/2","2/3/3","2/4/1","2/4/2","2/4/3"],
    ?assertEqual("2/1/1", tms:url_corner(Assert, top_left)).

url_corner_br_test()->
    Assert = ["2/1/1","2/1/2","2/1/3","2/2/1","2/2/2","2/2/3","2/3/1","2/3/2","2/3/3","2/4/1","2/4/2","2/4/3"],
    ?assertEqual("2/4/3", tms:url_corner(Assert, bottom_right)).
%%
%%
move_left_test()->
    %% Move 1 time to the left
    Assert = ["2/0/1","2/0/2","2/0/3","2/0/4"],
    ?assertEqual(Assert, tms:move("2/1/1",[4,4], 1, left)).

move_right_test()->
    %% Move 1 time to the right, add 1 to X
    Size = [3, 4],
    Assert = ["2/4/1","2/4/2","2/4/3","2/4/4"],
    ?assertEqual(Assert, tms:move("2/1/1", Size, 1, right)).

move_bottom_test()->
    %% Move 1 time to the bottom, add 1 to Y
    Size = [3, 2],
    Assert = ["4/5/6","4/6/6","4/7/6"],   
    ?assertEqual(Assert, tms:move("4/5/4", Size, 1, bottom)).

move_top_test()->
    %% Move 1 time to the top, sub 1 from Y
    Assert = ["6/1/0","6/2/0","6/3/0","6/4/0","6/5/0"],
    ?assertEqual(Assert, tms:move("6/1/1",[5,4], 1, top)).

move_top2_test()->
    %% Move 2 time to the top, sub 2 from Y
    Assert = ["6/1/6","6/2/6","6/3/6","6/4/6","6/5/6"],
    ?assertEqual(Assert, tms:move("6/1/8",[5,4], 2, top)).

move_top3_test()->
    %% Move 12 time to the top, sub 12 from Y
    %% move is over limit
    Assert = ["6/1/0","6/2/0","6/3/0","6/4/0","6/5/0"],
    ?assertEqual(Assert, tms:move("6/1/8",[5,4], 12, top)).
%%
%%
%% North, south, east, west
%%
move_north_test()->
    %% Move 1 time to the north
    %% The map is a 2x2 square
    DynVars = ts_dynvars:new([map_width, map_height, list_url], 
                             [<<"400">>,<<"400">>, ["8/42/50", "8/43/50", 
                                                    "8/42/51", "8/43/51"]
                             ]),
    Assert = ["8/42/49", "8/43/49"],
    ?assertEqual(Assert, tms:move_north({4, DynVars})).

move_south_test()->
    %% Move 1 time to the north
    %% The map is a 2x2 square
    DynVars = ts_dynvars:new([map_width, map_height, list_url], 
                             [<<"400">>,<<"400">>, ["8/42/50", "8/43/50", 
                                                    "8/42/51", "8/43/51"]
                             ]),
    Assert = ["8/42/52", "8/43/52"],
    ?assertEqual(Assert, tms:move_south({4, DynVars})).

move_west_test()->
    %% Move 1 time to the north
    %% The map is a 2x2 square
    DynVars = ts_dynvars:new([map_width, map_height, list_url], 
                             [<<"400">>,<<"400">>, ["8/42/50", "8/43/50", 
                                                    "8/42/51", "8/43/51"]
                             ]),
    Assert = ["8/41/50", "8/41/51"],
    ?assertEqual(Assert, tms:move_west({4, DynVars})).

move_east_test()->
    %% Move 1 time to the north
    %% The map is a 2x2 square
    DynVars = ts_dynvars:new([map_width, map_height, list_url], 
                             [<<"400">>,<<"400">>, ["8/42/50", "8/43/50", 
                                                    "8/42/51", "8/43/51"]
                             ]),
    Assert = ["8/44/50", "8/44/51"],
    ?assertEqual(Assert, tms:move_east({4, DynVars})).
%%
%%
%%
url_add_layers()->
    Layers = ["alpha","beta"],
    Urls = ["lorem","ipsum"],
    Attend = ["alpha/lorem","beta/lorem","beta/lorem","beta/lorem"],
    ?assertEqual(Attend, tms:url_add_layers(Layers, Urls)).

%%
%%
%%
move_north_layers_test()->
    %% Move 1 time to the north on all layers
    %% The map is a 2x2 square
    DynVars = ts_dynvars:new([map_width, map_height, list_url, tms_layers], 
                             [<<"400">>,<<"400">>,
			      ["8/42/50", "8/43/50", "8/42/51", "8/43/51"],
			      "foo,bar"]),
    Assert = ["foo/8/42/49", "bar/8/42/49", "foo/8/43/49", "bar/8/43/49"],
    ?assertEqual(Assert, tms:move_north_layers({4, DynVars})).

move_south_layers_test()->
    %% Move 1 time to the south on all layers
    %% The map is a 2x2 square
    DynVars = ts_dynvars:new([map_width, map_height, list_url, tms_layers], 
                             [<<"400">>,<<"400">>,
			      ["8/42/50", "8/43/50", "8/42/51", "8/43/51"],
			      "foo,bar"]),
    Assert = ["foo/8/42/52", "bar/8/42/52", "foo/8/43/52", "bar/8/43/52"],
    ?assertEqual(Assert, tms:move_south_layers({4, DynVars})).

move_west_layers_test()->
    %% Move 1 time to the west on all layers
    %% The map is a 2x2 square
    DynVars = ts_dynvars:new([map_width, map_height, list_url, tms_layers], 
                             [<<"400">>,<<"400">>,
			      ["8/42/50", "8/43/50", "8/42/51", "8/43/51"],
			      "foo,bar"]),
    Assert = ["foo/8/41/50", "bar/8/41/50", "foo/8/41/51", "bar/8/41/51"],
    ?assertEqual(Assert, tms:move_west_layers({4, DynVars})).

move_east_layers_test()->
    %% Move 1 time to the east on all layers
    %% The map is a 2x2 square
    DynVars = ts_dynvars:new([map_width, map_height, list_url, tms_layers], 
                             [<<"400">>,<<"400">>,
			      ["8/42/50", "8/43/50", "8/42/51", "8/43/51"],
			      "foo,bar"]),
    Assert = ["foo/8/44/50", "bar/8/44/50", "foo/8/44/51", "bar/8/44/51"],
    ?assertEqual(Assert, tms:move_east_layers({4, DynVars})).


