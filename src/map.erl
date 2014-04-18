%
% @author Rodolphe Quiedeville <rodolphe@quiedeville.org>
%   [http://rodolphe.quiedeville.org]
%
% @copyright 2014 Rodolphe Quiedeville
%
% @doc
% Dynamic variables :
%  - map_systems
% @end
-module(map).
-export([move_first/1]).

% @doc Convert list_url DynVar to list of coord
%
%
move_first({_Pid, DynVars})->
    Layers = laysplit(map_types(DynVars)),
    lists:merge(case lists:member("tms", Layers) of
                    true -> tms:move_first({_Pid, DynVars});
                    _ -> []
                end,
                case lists:member("wms", Layers) of
                    true -> wms:move_first({_Pid, DynVars});
                    _ -> []
                end).

map_types(DynVars)->
    laysplit(case ts_dynvars:lookup(map_systems, DynVars) of
                 {ok, Proj} -> Proj;
                 false -> "tms"
             end).

laysplit(Url)->
    string:tokens(Url, ":").
