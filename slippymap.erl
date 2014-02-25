
%
% @doc OpenStreetMap slippy map tile numbers
%
% @author Rodolphe Quiedeville <rodolphe@quiedeville.org>
%   [http://rodolphe.quiedeville.org]
%
% @copyright 2013,2014 Rodolphe Quiedeville
%
% @reference <a href="http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames">http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames</a>

-module(slippymap).
-export([deg2num/3]).
-export([num2deg/3,num2bbox/3]).
-export([tmstowms/1]).
-export([tile2lat/2,tile2lon/2]).

% @doc Convert list_url DynVar to list of coord
%
%
tmstowms({_Pid, DynVars})->
    Urls = lists:map(fun(Url)->
                             [Z, X, Y] = url_split(Url),
                             num2bbox(X, Y, Z)
                     end,
                     last_block(DynVars)),
    lists:usort(Urls).

last_block(DynVars)->
    case ts_dynvars:lookup(list_url, DynVars) of
        {ok, Block} -> Block;
        false -> ""
    end.

% @doc convert geometric coordinate to tile numbers
%
% @spec deg2num(Lat::float(), Lon:: float(), Zoom::integer()) -> {integer(), integer()}
%
deg2num(Lat,Lon,Zoom)->
    X = math:pow(2, Zoom) * ((Lon + 180) / 360),
    Sec = 1 / math:cos(deg2rad(Lat)),
    R = math:log(math:tan(deg2rad(Lat)) + Sec) / math:pi(),
    Y = math:pow(2, Zoom) * (1 - R) / 2,
    {round(X), round(Y)}.

% @doc convert tile numbers to geometric coordinate
%
% @spec num2deg(X::integer(), Y::integer(), Zoom::integer()) -> {float(), float()}
%
num2deg(X,Y,Zoom)->
    {tile2lon(X, Zoom), tile2lat(Y, Zoom)}.

deg2rad(C)->
    C * math:pi() / 180.

% @doc convert tile numbers to bouding box
%
%
num2bbox(X,Y,Zoom)->
    North = tile2lat(Y, Zoom),
    South = tile2lat(Y + 1, Zoom),
    West = tile2lon(X, Zoom),
    East = tile2lon(X + 1, Zoom),
    {West, South, East, North}.

tile2lat(Y, Z)->
    % double n = Math.PI - (2.0 * Math.PI * y) / Math.pow(2.0, z);
    % return Math.toDegrees(Math.atan(Math.sinh(n)));) ->
    N = math:pi() - ( 2.0 * math:pi() * Y) / math:pow(2, Z),
    LatRad = math:atan(math:sinh(N)),
    LatRad * 180 / math:pi().

tile2lon(X, Z)->
    % double n = Math.PI - (2.0 * Math.PI * y) / Math.pow(2.0, z);
    % return Math.toDegrees(Math.atan(Math.sinh(n)));) ->
    N = math:pow(2, Z),
    Lon = X / N * 360 - 180,
    Lon.

%
%
%
%% Split the url
%%
%% @doc return an array with [Z, X, Y]
url_split(Url)->
    lists:map(fun(X) ->
                      {Int, _} = string:to_integer(X),
                      Int
              end,
              split(Url)).

split(Url)->
    Elmts = string:tokens(Url, "/"),
    case length(Elmts) of
        3 -> T=Elmts;
        _ -> [_|T] = Elmts
    end,
    T.
