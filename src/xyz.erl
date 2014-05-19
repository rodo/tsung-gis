%% -*- coding: utf-8 -*-
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
%% Generate tile map urls based on predictible actions
%%
%% @doc List of dynvars used:
%%  - first_url
%%  - list_url
%%  - map_height (in pixel)
%%  - map_width (in pixel)
%%  - tms_layers (optionnal)
%% @end
-module(xyz).
-export([urlzxy/1]).
-export([get_urlblock/1]).
-export([get_urlfrom/2]).
-export([move_first/1,move_first_layers/1,move_next/1]).
-export([move_north/1, move_south/1, move_west/1, move_east/1]).
-export([move_north_layers/1,move_south_layers/1,move_west_layers/1,move_east_layers/1]).

-ifdef(TEST).
-export([pixel2tiles/2]).
-endif.

-author({author, "Rodolphe Quiédeville", "<rodolphe@quiedeville.org>"}).

-define(ZOOM_LEVEL_MIN, 1).
-define(ZOOM_LEVEL_MAX, 18).

-define(TILE_WIDTH, 256).
-define(TILE_HEIGHT, 256).

-define(MAP_WIDTH, 800). %% nb tiles : 4
-define(MAP_HEIGHT, 600). %% nb tiles : 3

-define(PERCENT_ACTION_MOVE, 70).

urlzxy({_Pid, _DynVars})->
    {N1,N2,N3} = now(),
    random:seed(N1,N2,N3),
    %% Zoom level
    Arr = fillall(),
    Key = random:uniform(elmts(?ZOOM_LEVEL_MAX))-1,
    Zoomlevel = array:get(Key, array:from_list(Arr)),
    string:concat(zoomlevel(Zoomlevel), coord(Zoomlevel)).

%% The sizes are defined in dynvars or return default value
read_ssize(DynVars, height)->
    case ts_dynvars:lookup(map_height,DynVars) of
        {ok,Size} -> trunc(binary_to_number(Size) / ?TILE_HEIGHT) + 1;
        false -> pixel2tiles(?MAP_HEIGHT, ?TILE_HEIGHT)
    end;
read_ssize(DynVars, width)->
    case ts_dynvars:lookup(map_width,DynVars) of
        {ok,Size} -> trunc(binary_to_number(Size) / ?TILE_WIDTH) + 1;
        false -> pixel2tiles(?MAP_WIDTH, ?TILE_WIDTH)
    end.

%% Number of tiles
pixel2tiles(_, 0)->
    1;
pixel2tiles(MapWidth, TileSize)->
    trunc(MapWidth / TileSize) + 1.

%% @doc The first move
%%
%%
move_first({_Pid, DynVars})->
    case ts_dynvars:lookup(first_url, DynVars) of
        {ok, Url} ->
            [Z, X, Y] = url_split(Url),
            get_urlfrom(get_square_size(DynVars), [Z, X , Y]);
        false -> get_urlblock({"", DynVars})
    end.

%% @doc Can't be the first move, the next move is related to the
%% previous and read DynVars
%%
%% The next move is random from 6 possible actions
%%
%%
%%
move_next({_Pid, DynVars})->
    Sq=get_square_size(DynVars),
    TopLeft = url_corner(last_block(DynVars), top_left),
    case random_action(random:uniform(100)) of
        move -> move(TopLeft, Sq, 1, random_move());
        zoom -> zoom_move(TopLeft, Sq, random_zoom())
    end.
%%
%% Return urls of all layers to the north
%%
layers(DynVars)->
    case ts_dynvars:lookup(tms_layers, DynVars) of
        {ok, Layers} -> string:tokens(Layers, ",");
        false -> ""
    end.

url_add_layers(Layers, Urls)->
    lists:merge(lists:map(fun(X) -> url_add_layer(Layers, X) end, Urls)).

url_add_layer(Layers, Url)->
    lists:map(fun(X) -> X ++ "/" ++ Url end, Layers).
%% @doc The first move on all layers
%%
%%
move_first_layers({_Pid, DynVars})->
    Urls = move_first({_Pid, DynVars}),
    url_add_layers(layers(DynVars), Urls).

%% @doc Move to the north on all layers
%%
%%
move_north_layers({_Pid, DynVars})->
    Urls = move_north({_Pid, DynVars}),
    url_add_layers(layers(DynVars), Urls).

move_south_layers({_Pid, DynVars})->
    Urls = move_south({_Pid, DynVars}),
    url_add_layers(layers(DynVars), Urls).

move_west_layers({_Pid, DynVars})->
    Urls = move_west({_Pid, DynVars}),
    url_add_layers(layers(DynVars), Urls).

move_east_layers({_Pid, DynVars})->
    Urls = move_east({_Pid, DynVars}),
    url_add_layers(layers(DynVars), Urls).

%% @doc Move to the north
%% return list
%% @end
%%
move_north({_Pid, DynVars})->
    Sq=get_square_size(DynVars),
    TopLeft = url_corner(last_block(DynVars), top_left),
    move(TopLeft, Sq, top).
%%
%% Move to the south
%%
move_south({_Pid, DynVars})->
    Sq=get_square_size(DynVars),
    TopLeft = url_corner(last_block(DynVars), top_left),
    move(TopLeft, Sq, bottom).

%%
%% Move to the west
%%
move_west({_Pid, DynVars})->
    Sq=get_square_size(DynVars),
    TopLeft = url_corner(last_block(DynVars), top_left),
    move(TopLeft, Sq, 1, left).
%%
%% Move to the east
%%
move_east({_Pid, DynVars})->
    Sq=get_square_size(DynVars),
    TopLeft = url_corner(last_block(DynVars), top_left),
    move(TopLeft, Sq, 1, right).

%% @doc Random action return move or zoom
%%
%% @spec random_action( integer() ) -> string()
%%
random_action(X) when X =< ?PERCENT_ACTION_MOVE ->
    move;
random_action(X) when X > ?PERCENT_ACTION_MOVE ->
    zoom.

%% Return the last block
%%
%%
last_block(DynVars)->
    case ts_dynvars:lookup(list_url, DynVars) of
        {ok, Block} -> Block;
        false -> get_urlblock({"",DynVars})
    end.

%% @doc Return an array of urls
%%
%%
get_urlblock({_Pid, DynVars})->
    [Z,X,Y] = zxy(),
    Sq=get_square_size(DynVars),
    [Width, Height] = Sq,
    fillurls(0,[],Z,X,Y,Width,Height).

%% @doc Return the square side with limits
%%
%% If the square size is not defined in the scenario the default value
%% is returned
%%
%% For security reason a square can't be greater than 8 by 8
%%
%% @end
%%
get_square_size(DynVars)->
    Min = 1,
    Max = 8,
    [max(Min,min(read_ssize(DynVars, width), Max)),
     max(Min,min(read_ssize(DynVars, height), Max))].

%% Get an array of url from the bottom left corner
%%
%%
%%
get_urlfrom(Size, [Z,X,Y])->
    [Width, Height] = Size,
    fillurls(0,[],Z,X,Y,Width,Height).

fillurls(N,List,Z,X,Y,Width,Height) when N < Height->
    lists:merge(fillurls(N + 1,List,Z,X,Y,Width,Height),arr_urlzxy(0,N,Width,Z,X,Y));
fillurls(_,_,_,_,_,_,_)->
    [].

arr_urlzxy(L,N,B,Z,X,Y) when L < B->
    A=[integer_to_list(Z),integer_to_list(X + L),integer_to_list(Y + N)],
    lists:merge(arr_urlzxy(L + 1,N,B,Z,X,Y),[string:join(A,"/")]);
arr_urlzxy(_,_,_,_,_,_)->
    [].

zxy()->
    {N1,N2,N3} = now(),
    random:seed(N1,N2,N3),
    %% Zoom level
    Arr = fillall(),
    Key = random:uniform(171) - 1,
    Zoomlevel = array:get(Key, array:from_list(Arr)),
    [Zoomlevel,randxy(Zoomlevel),randxy(Zoomlevel)].

zoomlevel(N)->
    string:concat(integer_to_list(N), "/").

coord(N)->
    string:concat(coordx(N), coordy(N)).

coordx(N)->
    X = randxy(N),
    string:concat(integer_to_list(X), "/").

coordy(N)->
    Y = randxy(N),
    integer_to_list(Y).

randxy(N)->
    {N1,N2,N3} = now(),
    random:seed(N1,N2,N3),
    random:uniform(trunc(math:pow(2, N)))-1.

%% @doc Random move, choose a move randomly between 4 action
%%
%%
random_move()->
    case random:uniform(4) of
        1 -> left;
        2 -> bottom;
        3 -> right;
        4 -> top
    end.
%%
%%
%% @spec random_zoom( integer() ) -> integer()
%%
random_zoom()->
    case random:uniform(2) of
        1 -> more;
        2 -> less
    end.

%% Move to the direction : Orientation
%%
%%
move(Url, Size, Orientation)->
    move(Url, Size, 1, Orientation).

%% Move action are one of 4 :
%%
move(Url, Size, Value, left)->
    [_,H] = Size,
    [Z, X, Y] = url_split(Url),
    get_urlfrom([1,H], [Z, max(0, X - Value), Y]);
move(Url, Size, Value, bottom)->
    [W,H] = Size,
    [Z, X, Y] = url_split(Url),
    get_urlfrom([W,1], [Z, X, Y + Value + H - 1]);
move(Url, Size, Value, right)->
    [W,H] = Size,
    [Z, X, Y] = url_split(Url),
    get_urlfrom([1,H], [Z, X + Value + W - 1, Y]);
move(Url, Size, Value, top)->
    [W,_] = Size,
    [Z, X, Y] = url_split(Url),
    get_urlfrom([W,1], [Z, X, max(0,Y - Value)]).

zoom_move(Url, Size, Operation)->
    [Z, X, Y] = url_split(Url),
    NewZoom = new_zoom(Z, Operation),
    get_urlfrom(Size, [NewZoom,
                       coord_zoom(X, Z, NewZoom),
                       coord_zoom(Y, Z, NewZoom)]).

%% @doc Calculate the related tile number between two zoom level
%%
%%
coord_zoom(X, OldZoom, NewZoom) when OldZoom > NewZoom->
    coord_zoom(round(X / 2), OldZoom - 1, NewZoom);
coord_zoom(X, OldZoom, NewZoom) when OldZoom < NewZoom->
    coord_zoom(2 * X ,OldZoom + 1, NewZoom);
coord_zoom(X, OldZoom, NewZoom) when OldZoom == NewZoom->
    X.

%% If the limit is reached return a random zoom level
%%
%%
new_zoom(Z, more) when Z == ?ZOOM_LEVEL_MAX ->
    max(?ZOOM_LEVEL_MIN, random:uniform(?ZOOM_LEVEL_MAX) - 1);
new_zoom(Z, more) ->
    Z + 1;
new_zoom(Z, less) when Z == ?ZOOM_LEVEL_MIN ->
    max(?ZOOM_LEVEL_MIN, random:uniform(?ZOOM_LEVEL_MAX) - 1);
new_zoom(Z, less) ->
    Z - 1.

%%
%%
%%
url_corner(Urls, top_left)->
    [TopLeft|_] = Urls,
    TopLeft;
url_corner(Urls, bottom_right)->
    lists:nth(length(Urls), Urls).

%% Split the url
%%
%% @doc return an array with [Z, X, Y]
url_split(Url)->
    lists:map(fun(X) -> {Int, _} = string:to_integer(X),
                        Int end,
              split(Url)).

split(Url)->
    Elmts = string:tokens(Url, "/"),
    case length(Elmts) of
	3 -> T=Elmts;
	_ -> [_|T] = Elmts
    end,
    T.

%%======================================================================
%% Function: fillall/0
%% Purpose: return an array filled with N element at index N
%% Returns: []
%%======================================================================

fillall()->
    fillall(1, []).

fillall(N, List) when N =< ?ZOOM_LEVEL_MAX->
    lists:merge(fillall(N + 1, List),lists:seq(N, ?ZOOM_LEVEL_MAX));
fillall(_, _)->
    [].

elmts(N) when N >=1->
    elmts(N - 1) + N;
elmts(_)->
    0.

%% utilities
binary_to_number(B) when is_binary(B)->
    list_to_number(binary_to_list(B));
binary_to_number(B) when is_list(B)->
    list_to_number(B);
binary_to_number(B) ->
    B.

list_to_number(L) ->
    try list_to_float(L)
    catch
        error:badarg ->
            list_to_integer(L)
    end.
