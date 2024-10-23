%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% A simple `gen_xml' callback module to store the xml document in an
%%% ETS table. An entire XML document can be scanned and saved with
%%% `genxml_ets:read/1'.
%%%
%%% The module will create an ETS table and return the table ID at the
%%% end of the scan.
%%%
%%% The table entries (tuples) will contain the following elements:
%%%
%%% <ol>
%%% <li>`id': unique integer for the record</li>
%%%
%%% <li>`tag': the tag of the XML element, or the atom `$text'</li>
%%%
%%% <li>`attr': the map of attributes for the element</li>
%%%
%%% <li>`parent': the immediate container of this element, 0 =>
%%% root</li>
%%%
%%% <li>`text': the contents of an element, `parent' identifies the
%%% containing element</li>
%%%
%%% </ol>
%%%
%%% The state variable for this handler is a list of the currently
%%% open nested elements. The head of the list is the innermost
%%% element.
%%%
%%% @end
%%% Created : 23 Oct 2024 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(genxml_ets).

-behaviour(gen_xml).

-include_lib("kernel/include/logger.hrl").

-export([read/1]).

-export([handle_begin/3, handle_end/2, handle_text/2]).

%%--------------------------------------------------------------------

-record(handler, {tabid, path=[]}).

%%--------------------------------------------------------------------
%% @doc Read and save the contents of an XML file into an ETS table.
%%
%% Once the entire document is scanned the ETS table id is returned.
%%
%% A single dummy root record is inserted into the table. The dummy
%% record has a tag of `$root' and Id of of 0.
%%
%% @end
%%--------------------------------------------------------------------
-spec read(file:filename()) -> ets:table().
read(File) ->
    Tab_id = ets:new(?MODULE, [ordered_set]),
    Id  = 0,
    Tag = '$root',
    Rec = {Id, Tag, #{}, Id, []},
    ets:insert(Tab_id, Rec), %% add dummy root entry
    gen_xml:read(File, ?MODULE, #handler{tabid=Tab_id, path=[{Id, Tag}]}),
    Tab_id.

%%--------------------------------------------------------------------
%% @doc The callback function for begin tags.
%%
%% Each call insert the tag and its attributes into the ETS table.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_begin(atom(), list(), tuple()) -> tuple().
handle_begin(Tag, Attr, State=#handler{tabid=Tab_id, path=[{Id0, _}|_]=Path}) ->
    ?LOG_INFO("State=~p.", [State]),
    Id = erlang:unique_integer([positive]),
    Rec = {Id, Tag, gen_xml:attr_map(Attr), Id0, []},
    ets:insert(Tab_id, Rec),
    State#handler{path=[{Id,Tag}|Path]}.

%%--------------------------------------------------------------------
%% @doc The callback function for end tags.
%%
%% We expect `Tag' to match the latest element (head) of the path. If
%% not, the xmerl parser would have complained.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_end(atom(), tuple()) -> tuple().
handle_end(Tag, State=#handler{path=[{_,Tag}|Path]}) ->
    ?LOG_INFO("State=~p.", [State]),
    State#handler{path=Path}.

%%--------------------------------------------------------------------
%% @doc The callback function for text elements.
%%
%% The text of the element body is inserted into the ETS table. We use
%% the dummy tag `$text' for the entry.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_text(string(), tuple()) -> tuple().
handle_text(Text, State=#handler{tabid=Tab_id, path=[{Id0, _}|_]}) ->
    ?LOG_INFO("State=~p.", [State]),
    Id = erlang:unique_integer([positive]),
    Tag = '$text',
    Rec = {Id, Tag, #{}, Id0, Text},
    ets:insert(Tab_id, Rec),
    State.

%%--------------------------------------------------------------------
