

-module(fill).
-export([url/0]).


url()->
    foo(["toto","titi"], ["64", "66","67"]).


foo(Arr, Boo)->
    lists:merge(lists:map(fun(X) -> bar(Boo, X) end, Arr)).

bar(Arr, Boo)->
    lists:map(fun(X) -> X ++ "/" ++ Boo end, Arr).

