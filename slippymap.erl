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
-export([num2deg/3]).

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
    N = math:pow(2, Zoom),
    Lon = X / N * 360 - 180,
    Lat_rad = math:atan(math:sinh(math:pi() * (1 - 2 * Y / N))),
    Lat = Lat_rad * 180 / math:pi(),
    {Lon, Lat}.

deg2rad(C)->
    C * math:pi() / 180.
