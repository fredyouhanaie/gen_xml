%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% `genxml' is a command line script for experimenting with the
%%% various example callback modules.
%%%
%%% @end
%%% Created : 2024-10-13 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(genxml).

%% API exports
-export([main/1]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->

    %% set up default logger (single line)
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    logger:set_primary_config(level, error),

    %% scan the args and run
    %% argparse:run(Args, cli(), #{progname => genxml}),

    Parsed = argparse:parse(Args, cli(), #{progname => genxml}),
    case Parsed of
        {error, Error} ->
            ?LOG_ERROR(argparse:format_error(Error));
        {ok, Arg_map, Path, Command} ->
            run(Arg_map, Path, Command)
    end,

    timer:sleep(100), %% give the logger a chance to flush all the messages!!
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-define(Arguments,
        [ #{ name => verbose, long => "-verbose", short => $v,
             type => boolean, action => count,
             help => "be verbose, can use multiple times for warning to debug" },
          #{ name => help, long => "-help", short => $h,
             type => boolean,
             help => "display help/usage information" },
          #{ name => file, nargs => 1 }
        ] ).

-define(Commands,
        #{ "null"    => #{ help => "run the null callback module",
                           handler => fun do_null/1 },
           "counts"  => #{ help => "run the counts callback module",
                           handler => fun do_counts/1 },
           "ets"     => #{ help => "run the ets callback module",
                           handler => fun do_ets/1 },
           "paths"   => #{ help => "run the paths callback module",
                           handler => fun do_paths/1 }
         } ).

cli() ->
    #{ arguments => ?Arguments,
       commands  => ?Commands
     }.

%%--------------------------------------------------------------------
%% run a subcommand
%%
run(Arg_map, Path, Command) ->
    %% check/set the verbosity
    Level = case maps:get(verbose, Arg_map, 0) of
                0 -> error;
                1 -> warning;
                2 -> notice;
                3 -> info;
                _ -> debug
            end,
    logger:set_primary_config(level, Level),
    ?LOG_DEBUG(#{ arg_map => Arg_map,
                  path    => lists:join($/, Path),
                  command => Command }),

    erlang:apply(map_get(handler, Command), [Arg_map]),

    ok.

%%--------------------------------------------------------------------

do_null(Args) ->

    File = map_get(file, Args),
    Result = genxml_null:start(File),
    io:format("~p.~n", [Result]),
    ok.

%%--------------------------------------------------------------------

do_counts(Args) ->

    File = map_get(file, Args),
    Result = genxml_counts:start(File),
    {ok, Counts} = Result,

    Print = fun (Tag, Count) -> io:format("~8w,~s~n", [Count, Tag]) end,
    maps:foreach(Print, Counts),

    ok.

%%--------------------------------------------------------------------

do_ets(Args) ->

    File = map_get(file, Args),
    Result = genxml_ets:read(File),
    {ok, Tab_id} = Result,

    Data = ets:tab2list(Tab_id),
    io:format("~p~n", [Data]),

    ok.

%%--------------------------------------------------------------------

do_paths(Args) ->
    File = map_get(file, Args),
    genxml_paths:print(File).

%%--------------------------------------------------------------------
