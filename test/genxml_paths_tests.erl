%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2025, Fred Youhanaie
%%% @doc
%%%
%%% Run the EUnit tests for the gen_xml `paths' callback module.
%%%
%%% @end
%%% Created : 2025-01-04 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(genxml_paths_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("gen_xml_tests.hrl").

%% Change `Log_level' if investigating failed tests
-define(Log_level, critical).

%%--------------------------------------------------------------------

%% genxml_paths expects an empty map for intial state
-define(Path_fun, fun (_Path) -> ok end).

-define(Init_state, {?Path_fun, []}).

-define(Paths_sample_4, []).

-define(Paths_sample_5, []).

-define(Paths_sample_6, []).

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
       ?_assertMatch({error, _Reason}, gen_xml:read(?Doc_nofile, genxml_paths, ?Init_state))},
      {"empty file",
       ?_assertMatch({fatal_error, _, _, _, _}, gen_xml:read(?Doc_empty, genxml_paths, ?Init_state))}
     ]}.

%%--------------------------------------------------------------------

read_sample_2_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [ {"sample-4",
        ?_assertMatch({ok, {_, ?Paths_sample_4}}, gen_xml:read(?Doc_sample_4, genxml_paths, ?Init_state))},
       {"sample-5",
        ?_assertMatch({ok, {_, ?Paths_sample_5}}, gen_xml:read(?Doc_sample_5, genxml_paths, ?Init_state))},
       {"sample-6",
        ?_assertMatch({ok, {_, ?Paths_sample_6}}, gen_xml:read(?Doc_sample_6, genxml_paths, ?Init_state))}
     ]}.

%%--------------------------------------------------------------------

print_1_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [ {"no file",
       ?_assertMatch({error, _Reason}, genxml_paths:print(?Doc_nofile))},
       {"empty file",
        ?_assertMatch({fatal_error, _, _, _, _}, genxml_paths:print(?Doc_empty))},
       {"sample-4",
        ?_assertEqual({ok, ?Paths_sample_4}, genxml_paths:print(?Doc_sample_4))},
       {"sample-5",
        ?_assertEqual({ok, ?Paths_sample_5}, genxml_paths:print(?Doc_sample_5))},
       {"sample-6",
        ?_assertEqual({ok, ?Paths_sample_6}, genxml_paths:print(?Doc_sample_6))}
     ] }.

%%--------------------------------------------------------------------
