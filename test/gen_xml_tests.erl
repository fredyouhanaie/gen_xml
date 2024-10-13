%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2024, Fred Youhanaie
%%% @doc
%%%
%%% Run the EUnit tests for the pnml module.
%%%
%%% @end
%%% Created : 2024-10-13 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(gen_xml_tests).

-include_lib("eunit/include/eunit.hrl").

-define(Doc_nofile, "test/no_file.xml").   %% this file should never exist!
-define(Doc_empty,  "test/empty_doc.xml"). %% keep this file empty

%% Change `Log_level' if investigating failed tests
-define(Log_level, critical).

%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

setup() ->
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    logger:set_primary_config(level, ?Log_level).

cleanup(_) ->
    ok.

%%--------------------------------------------------------------------

read_sample_1_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"no file",
       ?_assertMatch({error, _Reason}, gen_xml:read(?Doc_nofile, genxml_null, null))},
      {"empty file",
       ?_assertMatch({fatal_error, _, _, _, _}, gen_xml:read(?Doc_empty, genxml_null, null))}
     ]}.

%%--------------------------------------------------------------------
