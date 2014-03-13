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
-export([rcoord/0,rcoord/2,rcoord/4,rcoord_array/1]).
-export([url/1]).

url({_Pid,_DynVars})->
    {Lat,Lon} = rcoord(),
    "lat=" ++ lists:flatten(io_lib:format("~.6f",[Lat])) ++ "&lon=" ++ lists:flatten(io_lib:format("~.6f",[Lon])).

rcoord_array({_Pid,_DynVars})->
    {Lat,Lon} = rcoord(),
    [lists:flatten(io_lib:format("~.6f",[Lat])), 
     lists:flatten(io_lib:format("~.6f",[Lon]))].

rcoord()->
    rcoord(-90.0,90.0,-180.0,180.0).

rcoord(Bottom,Top,Left,Right)->
    Lon=longitude(float(Left),float(Right)),
    Lat=latitude(float(Bottom),float(Top)),
    {Lat,Lon}.

rcoord(Min,Max)->
    {N1,N2,N3} = now(),
    random:seed(N1,N2,N3),
    Val=random:uniform() + Min + random:uniform(round(Max - Min)),
    max(Min,min(Val,Max)).

longitude(Left,Right)->
    X = rcoord(Left,Right),
    min(180.0,X).

latitude(Min,Max)->
    X = rcoord(Min,Max),
    min(90.0,X).

