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
-module(store).
-export([url/1,ret/1]).

url({_Pid,DynVars})->
    "lat=" ++ foo(DynVars).

foo(DynVars)->
    case ts_dynvars:lookup(country, DynVars) of
        {ok, Value}-> ts_modules_cache:store(country, Value);
        false -> ""
    end.


ret({_Pid,_DynVars})->
    {ok, Vald}=ts_modules_cache:retrieve(country),
    Vald.
