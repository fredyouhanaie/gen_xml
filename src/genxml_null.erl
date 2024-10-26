%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% The gen_xml `null' callback module
%%%
%%% As the name implies, no processing is performed on the XML
%%% elements. This callback module is provided for testing and
%%% performance benchmarking.
%%%
%%% @end
%%% Created : 2024-10-13 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(genxml_null).

-behaviour(gen_xml).

-export([start/1]).

-export([handle_begin/3, handle_end/2, handle_text/2]).

%%--------------------------------------------------------------------
%% @doc Start the processing of an XML file.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(file:filename()) -> {ok, null}.
start(File) ->
    gen_xml:read(File, ?MODULE, null).

%%--------------------------------------------------------------------
%% @doc The null callbacks.
%%
%% The function does nothing, it just behaves as a "compliant callback
%% handler".
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_begin(atom(), term(), term()) -> term().
handle_begin(_Tag, _Attr, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc The null callbacks.
%%
%% The function does nothing, it just behaves as a "compliant callback
%% handler".
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_end(atom(), term()) -> term().
handle_end(_Tag, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc The null callbacks.
%%
%% The function does nothing, it just behaves as a "compliant callback
%% handler".
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_text(string(), term()) -> term().
handle_text(_Text, State) ->
    State.

%%--------------------------------------------------------------------
