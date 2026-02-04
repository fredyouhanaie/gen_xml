%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% A simple `gen_xml' callback module that produces the XML paths in
%%% the document. This similar to the `xmlstarlet el' command, see
%%% https://xmlstar.sourceforge.net/docs.php.
%%%
%%% The module will print the nested XML tags as `/' separated paths,
%%% showing the structure of the document.
%%%
%%% @end
%%% Created : 24 Oct 2024 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(genxml_paths).

-behaviour(gen_xml).

-export([print/1, collect/1]).

-export([handle_begin/3, handle_end/2, handle_text/2]).

%%--------------------------------------------------------------------
%% @doc Helper function to scan and print the paths of an XML document.
%%
%% @end
%%--------------------------------------------------------------------
-spec print(file:filename()) -> gen_xml:read_ret().
print(File) ->
    Print = fun (Path, Acc) -> io:format("~s~n", [Path]), Acc end,
    case gen_xml:read(File, ?MODULE, {Print, [], none}) of
        {ok, {Print, [], none}} ->
            {ok, []}; %% no need to return the function
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Helper function to scan and collect paths of an XML document.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect(file:filename()) -> gen_xml:read_ret().
collect(File) ->
    Collect = fun (Path, Acc) -> [Path|Acc] end,
    case gen_xml:read(File, ?MODULE, {Collect, [], []}) of
        {ok, {Collect, [], Paths}} ->
            {ok, Paths}; %% we just return the list of collected paths
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc The callback function for begin tags.
%%
%% Each call prints the paths collected so far.
%%
%% The attributes are ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_begin(atom(), list(), {function(), list(), term()})
                  -> {function(), list(), term()}.
handle_begin(Tag, _Attr, {F, Tags, Acc}) ->
    Tags_new = [Tag|Tags],
    Tags_str = [ atom_to_list(A) || A <- lists:reverse(Tags_new) ],
    Path = string:join(Tags_str, "/"),
    Acc_new = F(Path, Acc),
    {F, Tags_new, Acc_new}.

%%--------------------------------------------------------------------
%% @doc The callback function for end tags.
%%
%% We remove the first (inner tag) from the list.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_end(atom(), {function(), list(), term()}) ->
          {function(), list(), term()}.
handle_end(Tag, {F, [Tag|Tags_rest], Acc}) ->
    {F, Tags_rest, Acc}.

%%--------------------------------------------------------------------
%% @doc The callback function for text elements.
%%
%% No action is performed with these elements.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_text(string(), {function(), list(), term()})
                 -> {function(), list(), term()}.
handle_text(_Text, State) ->
    State.

%%--------------------------------------------------------------------
