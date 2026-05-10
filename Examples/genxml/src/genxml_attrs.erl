%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2026, Fred Youhanaie
%%% @doc
%%%
%%% A `gen_xml' callback module to collect the attribute names of the
%%% elements.
%%%
%%% We return a list of tuples (pairs) where the first element is an
%%% element tag name (atom) and the second is a list of attribute
%%% names (atoms).
%%%
%%% The state variable for this handler is a list of the
%%% element/attribute pairs.
%%%
%%% @end
%%% Created : 10 May 2026 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(genxml_attrs).

-behaviour(gen_xml).

-include_lib("kernel/include/logger.hrl").

-export([read/1]).

-export([handle_begin/3, handle_end/2, handle_text/2]).

%%--------------------------------------------------------------------

-type tag_attr() :: {atom(), [atom()]}.

%%--------------------------------------------------------------------
%% @doc Read and return the element/attr pairs.
%%
%% @end
%%--------------------------------------------------------------------
-spec read(file:filename()) -> {ok, [tag_attr()]}.
read(File) ->
    gen_xml:read(File, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc The callback function for begin tags.
%%
%% Each call inserts the tag and its attributes to the state variable.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_begin(atom(), list(), [tag_attr()]) -> [tag_attr()].
handle_begin(Tag, Attr, State) ->
    ?LOG_INFO("State=~p.", [State]),
    Attr_names = [list_to_atom(A) || {_, _, A, _} <- Attr],
    [{Tag, Attr_names}|State].

%%--------------------------------------------------------------------
%% @doc The callback function for end tags.
%%
%% No processing is performed here. We return the state variable
%% unchanged.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_end(atom(), [tag_attr()]) -> [tag_attr()].
handle_end(_Tag, State) ->
    ?LOG_INFO("State=~p.", [State]),
    State.

%%--------------------------------------------------------------------
%% @doc The callback function for text elements.
%%
%% No processing is performed here. We return the state variable
%% unchanged.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_text(string(), [tag_attr()]) -> [tag_attr()].
handle_text(_Text, State) ->
    ?LOG_INFO("State=~p.", [State]),
    State.

%%--------------------------------------------------------------------
