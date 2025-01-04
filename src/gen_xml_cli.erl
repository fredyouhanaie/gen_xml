%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2025, Fred Youhanaie
%%% @doc
%%%
%%% `gen_xml_cli' is a command line script for running the main
%%% callback modules in the `gen_xml' package.
%%%
%%% @end
%%% Created : 2025-01-03 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(gen_xml_cli).

%% API exports
-export([main/1]).

%% subcommand handlers
-export([ do_counts/1,
          do_paths/1,
          do_null/1
        ]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

-include_lib("include/gen_xml_cli.hrl").

-define(Progname, #{progname => gen_xml}).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->

    %% set up default logger (single line)
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    logger:set_primary_config(level, error),

    %% scan the args and run
    argparse:run(Args, cli(), ?Progname),

    timer:sleep(100), %% give the logger a chance to flush all the messages!!
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

cli() ->
    #{ arguments => ?Arguments_gen_xml,
       commands  => ?Commands_gen_xml
     }.

%%--------------------------------------------------------------------

do_null(Args) ->
    check_verbosity(Args),

    File = map_get(file, Args),
    Result = genxml_null:start(File),
    io:format("~p.~n", [Result]),
    ok.

%%--------------------------------------------------------------------

do_counts(Args) ->
    check_verbosity(Args),

    File = map_get(file, Args),
    Result = genxml_counts:start(File),
    {ok, Counts} = Result,

    Print = fun (Tag, Count) -> io:format("~8w,~s~n", [Count, Tag]) end,
    maps:foreach(Print, Counts),

    ok.

%%--------------------------------------------------------------------

do_paths(Args) ->
    check_verbosity(Args),

    File = map_get(file, Args),
    genxml_paths:print(File).

%%--------------------------------------------------------------------

check_verbosity(Args) ->
    %% check/set the verbosity
    Level = case maps:get(verbose, Args, 0) of
                0 -> error;
                1 -> warning;
                2 -> notice;
                3 -> info;
                _ -> debug
            end,
    logger:set_primary_config(level, Level),
    ?LOG_NOTICE(#{ arg_map => Args }).

%%--------------------------------------------------------------------
