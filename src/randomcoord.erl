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
%% Return a tuple Lat,Lon in range
%%
-module(randomcoord).
-export([url/1,rcoord_array/1]).
-export([rcoord/0,rcoord/1,rcoord/2,rcoord/4]).
-ifdef(TEST).
-export([longitude/2,latitude/2]).
-endif.

url({_Pid,_DynVars})->
    {Lat,Lon} = rcoord(),
    "lat=" ++ lists:flatten(io_lib:format("~.6f",[Lat])) ++ "&lon=" ++ lists:flatten(io_lib:format("~.6f",[Lon])).

rcoord_array({_Pid, DynVars})->
    case ts_dynvars:lookup(country, DynVars) of
        {ok,Value}->
	    {Lat,Lon} = rcoord(Value);
        false ->
	    {Lat,Lon} = rcoord()
    end,    
    [lists:flatten(io_lib:format("~.6f",[Lat])), 
     lists:flatten(io_lib:format("~.6f",[Lon]))].

rcoord("france")->
    rcoord(-0.8336,43.5771,5.9108,49.2775);
rcoord("spain")->
    rcoord(-6.14,38.4328,-0.3295,42.7499);
rcoord("portugal")->
    rcoord(-8.546,37.1392,-7.5914,41.7578);
rcoord("germany")->
    rcoord(7.0875,49.1931,12.0082,53.2182);
rcoord(_) ->
    rcoord().

%% Description : random coordinate
%% Returns : float
rcoord() ->
    rcoord(-180,-90.0,180.0,90.0).

%% Description : random coordinate in a bbox defined by 4 coordinate
%% Parameters : 
%% - Left : float
%% - Bottom : float
%% - Right : float
%% - Top : float
%%
%% Returns : {float, float}
rcoord(Left,Bottom,Right,Top)->
    Lon=longitude(float(Left),float(Right)),
    Lat=latitude(float(Bottom),float(Top)),
    {Lat,Lon}.

rcoord(Min,Max) when Min == Max ->
    float(Max);
rcoord(Min,Max) when Min > Max ->
    rcoord(Max, Min);
rcoord(Min,Max) ->
    {N1,N2,N3} = now(),
    random:seed(N1,N2,N3),
    Val=random:uniform() + Min + random:uniform(round(Max - Min)),
    float(max(Min,min(Val,Max))).
%%
%% Longitude
%% Returns : float
longitude(Left,Right)->
    X = rcoord(Left,Right),
    min(180.0,X).
%%
%% Latitude
%% Returns : float
%%
latitude(Min,Max)->
    X = rcoord(Min,Max),
    min(90.0,X).

