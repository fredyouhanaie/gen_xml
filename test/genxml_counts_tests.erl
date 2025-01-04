%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2025, Fred Youhanaie
%%% @doc
%%%
%%% Run the EUnit tests for the gen_xml `counts' callback module.
%%%
%%% @end
%%% Created : 2025-01-04 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(genxml_counts_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("gen_xml_tests.hrl").

%% Change `Log_level' if investigating failed tests
-define(Log_level, critical).

%%--------------------------------------------------------------------

%% genxml_counts expects an empty map for intial state
-define(Init_state, #{}).

-define(Counts_sample_4,
        #{name => 2,root => 1,title => 1,author => 1,age => 2,
          person => 2,email => 2,book => 1,year => 1}).

-define(Counts_sample_5,
        #{name => 16,root => 1,title => 8,author => 8,age => 16,
          person => 16,email => 16,book => 8,year => 8}).

-define(Counts_sample_6,
        #{name => 48,root => 1,title => 24,author => 24, age => 48,
          person => 48,email => 48,book => 24, year => 24}).

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
       ?_assertMatch({error, _Reason}, gen_xml:read(?Doc_nofile, genxml_counts, ?Init_state))},
      {"empty file",
       ?_assertMatch({fatal_error, _, _, _, _}, gen_xml:read(?Doc_empty, genxml_counts, ?Init_state))}
     ]}.

%%--------------------------------------------------------------------

read_sample_2_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [ {"sample-4",
        ?_assertEqual({ok, ?Counts_sample_4}, gen_xml:read(?Doc_sample_4, genxml_counts, ?Init_state))},
       {"sample-5",
        ?_assertEqual({ok, ?Counts_sample_5}, gen_xml:read(?Doc_sample_5, genxml_counts, ?Init_state))},
       {"sample-6",
        ?_assertEqual({ok, ?Counts_sample_6}, gen_xml:read(?Doc_sample_6, genxml_counts, ?Init_state))}
     ]}.

%%--------------------------------------------------------------------
