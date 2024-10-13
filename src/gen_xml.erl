%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% A set of functions to process `XML' files. See the overview doc
%%% for further details.
%%%
%%% To start the process of parsing the XML file, the callback module
%%% should call the `read/3' function. While `read/3' is scanning the
%%% XML file it will call the appropriate handler functions.
%%%
%%% The callback module should provide three callback functions:
%%%
%%% <ul>
%%%
%%% <li>`handle_begin/3' will be called whenever a start tag is
%%% encountered,</li>
%%%
%%% <li>`handle_end/2' will be called whenever an end tag is
%%% encountered.</li>
%%%
%%% <li>`handle_text/2' will be called whenever character data is
%%% seen.</li>
%%%
%%% </ul>
%%%
%%% All three callback handlers are passed the behaviour state, and should
%%% return the state, optionally updated.
%%%
%%% @end
%%% Created : 2024-10-12 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(gen_xml).

-export([read/3, attr_map/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/logger.hrl").

%% record type used for the SAX `user_state'
-record(state, {cb_module, cb_state}).

%%--------------------------------------------------------------------

-type read_ret() :: {ok, State :: term()} | {error, Reason :: term()}.
-export_type([read_ret/0]).

%%--------------------------------------------------------------------

-callback handle_begin(Tag ::  atom(), Attr :: term(), State :: term()) -> Result :: term().
-callback handle_end(Tag :: atom(), State :: term()) -> Result :: term().
-callback handle_text(Text :: string(), State :: term()) -> Result :: term().

%%--------------------------------------------------------------------
%% @doc Read in a valid XML file and process its elements using the
%% supplied callback module.
%%
%% We expect the file to be a valid XML document, no validation is
%% performed here. However, the supplied callback module can perform
%% its own validation during the scan.
%%
%% We return success/failure result. In case of success, the handler's
%% final state is returned. The failure reason may originate from the
%% `xmerl_sax_parser' module.
%%
%% @end
%%--------------------------------------------------------------------
-spec read(string(), atom(), term()) -> read_ret().
read(Filename, CB_module, CB_state) ->
    ?LOG_INFO("read: scan started File=~p.", [Filename]),

    Scan_opts = [{event_fun, fun event_cb/3},
                 {event_state, #state{cb_module=CB_module,
                                      cb_state=CB_state}}
                ],

    Result = case xmerl_sax_parser:file(Filename, Scan_opts) of
                 {ok, State, _Rest} ->
                     {ok, State#state.cb_state};
                 Other_result ->
                     Other_result
             end,
    %% collect the statistics and return the results
    ?LOG_INFO("read: Result=~p).", [Result]),
    Result.

%%--------------------------------------------------------------------
%% @doc The event handler for the SAX parser.
%%
%% We basically handle all the tag starts, ends and character
%% contents, and ignore the rest.
%%
%% With all callback handlers, `handle_begin', `handle_end' and
%% `handle_text', we supply the current state as the last argument to
%% the handler, and use the returned result as the new state.
%%
%% For the `startElement' event we call `handle_begin/3' with the
%% element's `Tag' as an atom, and `Attributes', as provided by
%% `xmerl_sax_parser' event data. The handler can use the function
%% `attr_map/1` to extract a map of `#{Key => Value}' pairs from
%% `Attributes'.
%%
%% For the `endElement' event we call `handle_end/2' with the `Tag'
%% and the current state.
%%
%% For the `characters' event we call `handle_text/2' with the element
%% text and the current state.
%%
%% @end
%%--------------------------------------------------------------------
-spec event_cb({atom(), string(), string(), string(), list()}, term(), term()) -> term().
event_cb({startElement, _Uri, LocalName, _QualName, Attr}, _Loc,
         #state{cb_module=CB_module, cb_state=CB_state0}=State) ->
    ?LOG_INFO("event_cb: startElement."),
    Tag = list_to_atom(LocalName),
    CB_state1 = CB_module:handle_begin(Tag, Attr, CB_state0),
    State#state{cb_state=CB_state1};

event_cb({endElement, _Uri, LocalName, _QualName}, _Loc,
         #state{cb_module=CB_module, cb_state=CB_state0}=State) ->
    ?LOG_INFO("event_cb: endElement."),
    Tag = list_to_atom(LocalName),
    CB_state1 = CB_module:handle_end(Tag, CB_state0),
    State#state{cb_state=CB_state1};

event_cb({characters, Text}, _Loc,
         #state{cb_module=CB_module, cb_state=CB_state0}=State) ->
    ?LOG_INFO("event_cb: characters."),
    CB_state1 = CB_module:handle_text(Text, CB_state0),
    State#state{cb_state=CB_state1};

event_cb(Event, _Loc, State) ->
    %% we ignore all the other events
    ?LOG_DEBUG("event_cb: IGNORED Ev=~p, St=~p.", [Event, State]),
    State.

%%--------------------------------------------------------------------
%% @doc Return a map corresponding to the list of attributes from
%% xmerl.
%%
%% We extract the attribute names and values, and ignore the rest.
%%
%% The attribute names are returned as atoms and the values as
%% strings.
%%
%% @end
%%--------------------------------------------------------------------
-spec attr_map(list()) -> map().
attr_map(Attr) ->
    Attr_to_pair = fun ({_, _, Name, Val}) -> {list_to_atom(Name), Val} end,
    Attr_list = lists:map(Attr_to_pair, Attr),
    maps:from_list(Attr_list).

%%--------------------------------------------------------------------
