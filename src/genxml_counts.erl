%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% A simple `gen_xml' callback module to count the tags.
%%%
%%% The module will return count of the element tags found in the XML
%%% document.
%%%
%%% The state variable for this handler should be a `map'. When
%%% calling `read/2', it is recommended to supply an empty map as the
%%% initial value, i.e. `#{}', although a map with preset values will
%%% not be rejected.
%%%
%%% If a key for the `Tag' does not exist a new entry will be
%%% created. If a non-numeric entry exists for the `Tag', the
%%% increment operation will cause an exception!
%%%
%%% @end
%%% Created : 14 Oct 2024 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(genxml_counts).

-behaviour(gen_xml).

-include_lib("kernel/include/logger.hrl").

-export([start/1]).

-export([handle_begin/3, handle_end/2, handle_text/2]).

%%--------------------------------------------------------------------
%% @doc Helper function to scan an entire XML document.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(string()) -> gen_xml:read_ret().
start(File) ->
    gen_xml:read(File, ?MODULE, #{}).

%%--------------------------------------------------------------------
%% @doc The callback function for begin tags.
%%
%% Each call increments the count for the corresponding `Tag'.
%%
%% Everything else, such as attributes, end tags and text are ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_begin(atom(), list(), map()) -> map().
handle_begin(Tag, _Attr, Counts) ->
    ?LOG_INFO("Counts=~p.", [Counts]),
    C = maps:get(Tag, Counts, 0),
    maps:put(Tag, C+1, Counts).

%%--------------------------------------------------------------------
%% @doc The callback function for end tags.
%%
%% No action is performed with end tags.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_end(atom(), map()) -> map().
handle_end(_Tag, Counts) ->
    Counts.

%%--------------------------------------------------------------------
%% @doc The callback function for text elements.
%%
%% No action is performed with these elements.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_text(string(), map()) -> map().
handle_text(_Text, Counts) ->
    Counts.

%%--------------------------------------------------------------------
