#!/usr/bin/escript
%% -*- erlang -*-
%%! -pa ebin-test/
main(_Args) ->
    eunit:test([test_all], [{report,{eunit_surefire,[{dir,"."}]}}]).
