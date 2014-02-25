%
% @doc Reproject Datas
%
% @author Rodolphe Quiedeville <rodolphe@quiedeville.org>
%   [http://rodolphe.quiedeville.org]
%
% @copyright 2013,2014 Rodolphe Quiedeville
%
% @reference https://github.com/greenelephantlabs/proj4erl
%
% @doc 
% Dynamic variables :
%  - wms_proj
% @end

-module(reproj).
-export([tmstowms/1]).
-export([reproj/3]).

% @doc Convert list_url DynVar to list of coord
%
%
tmstowms({_Pid, DynVars})->
    Urls = slippymap:tmstowms({_Pid, DynVars}),
    Proj = newproj(DynVars),
    case Proj of
        "epsg:4326" -> Urls;
        _ -> reproj("epsg:4326", Proj, Urls)
end.
                       
reproj(From, To, Datas)->             
    {ok, WGS84} = proj4:init("+init="++From),
    {ok, NewProj} = proj4:init("+init="++To),
    case is_list(Datas) of
        true -> P2 = lists:map(fun(X)-> 
                                       {ok, P} = proj4:transform(WGS84, NewProj, X), 
                                       P 
                               end, Datas);
        _ -> {ok, P2} = proj4:transform(WGS84, NewProj, Datas)
    end,
    P2.

newproj(DynVars)->
    case ts_dynvars:lookup(wms_proj, DynVars) of
        {ok, Proj} -> Proj;
        false -> ""
    end.
