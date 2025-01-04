%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% `genxml_cli' is a command line script for experimenting with the
%%% various example callback modules.
%%%
%%% @end
%%% Created : 2024-10-13 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(genxml_cli).

%% API exports
-export([main/1]).

%% subcommand handlers
-export([ do_ets/1 ]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

-include_lib("include/genxml_cli.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->

    %% set up default logger (single line)
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    logger:set_primary_config(level, error),

    %% scan the args and run
    argparse:run(Args, cli(), #{progname => genxml}),

    timer:sleep(100), %% give the logger a chance to flush all the messages!!
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

cli() ->
    #{ arguments => ?Arguments_genxml,
       commands  => ?Commands_genxml
     }.

%%--------------------------------------------------------------------

do_ets(Args) ->
    check_verbosity(Args),

    File = map_get(file, Args),
    Result = genxml_ets:read(File),
    {ok, Tab_id} = Result,

    Data = ets:tab2list(Tab_id),
    io:format("~p~n", [Data]),

    ok.

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
