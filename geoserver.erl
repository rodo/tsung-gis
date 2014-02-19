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
%%
%%
%%
-module(geoserver).
-export([urlwms/1]).
-export([defaults/0]).
-author({author, "Rodolphe Quiédeville", "<rodolphe@quiedeville.org>"}).

%%
%% Define here your defaults values
%%
-define(STYLES, "").
-define(TILESORIGIN, "").
-define(LAYERS, "").
-define(BBOX, "").
-define(SERVICE, "WMS").
-define(VERSION, "1.1.1").
-define(REQUEST, "GetMap").
-define(FORMAT, "image/png").
-define(TILED, "true").
-define(TRANSPARENT, "true").
-define(WIDTH, "256").
-define(HEIGHT, "256").
-define(SRS, "EPSG:900913").
-define(DEFAULTS,
        ts_dynvars:new([styles, service, version, request,
                        format, tiled, height, width,
                        transparent, srs,
                        layers, tilesorigin, bbox],
                       [?STYLES, ?SERVICE, ?VERSION, ?REQUEST,
                        ?FORMAT, ?TILED, ?HEIGHT, ?WIDTH,
                        ?TRANSPARENT, ?SRS,
                        ?LAYERS, ?TILESORIGIN, ?BBOX])).
%% http://c.tile.cartosm.eu/geoserver/wms?
%% LAYERS=DataGouv%3Arpg2010
%% TILESORIGIN=-20037508.34%2C-20037508.34
%% BBOX=288626.21876465,6525887.7259668,293518.18857422,6530779.6957764
%% STYLES=
%% TRANSPARENT=true
%% FORMAT=image%2Fpng
%% TILED=true
%% SERVICE=WMS
%% VERSION=1.1.1
%% REQUEST=GetMap
%% SRS=EPSG%3A900913
%% WIDTH=256
%% HEIGHT=256

urlwms({_Pid, DynVars})->
    string:strip(buildurl(DynVars, [format, styles, service,
                                    version, request, tiled,
                                    width, height, transparent,
                                    srs, layers, tilesorigin, bbox]), right, $&).

buildurl(_, []) ->
    "";
buildurl(DynVars, [H|T])->
    options(H, DynVars)++"&"++buildurl(DynVars,T).

%% The option value is defined in DynVars, return the dynvars
%%                                else, return the default value
%%
options(Option, DynVars)->
    case ts_dynvars:lookup(Option,DynVars) of
        {ok,Value}->
            code(Option) ++ "=" ++ encode(Value);
        false ->
            default_option(Option)
    end.

code(Value) when is_binary(Value) ->
    string:to_upper(binary_to_list(Value));
code(Value) when is_atom(Value) ->
    string:to_upper(atom_to_list(Value));
code(Value) ->
    Value.

encode(Value) when is_binary(Value) ->
    http_uri:encode(binary_to_list(Value));
encode(Value) when is_atom(Value) ->
    http_uri:encode(atom_to_list(Value));
encode(Value) when is_integer(Value) ->
    http_uri:encode(integer_to_list(Value));
encode(Value) ->
    http_uri:encode(Value).

default_option("")->
    "";
default_option(Option)->
    case ts_dynvars:lookup(Option,?DEFAULTS) of
        {ok,Value} -> string:to_upper(encode(Option)) ++ "=" ++ encode(Value);
        false -> "Error"
    end.

%% debug usage for developper
defaults()->
    ?DEFAULTS.
