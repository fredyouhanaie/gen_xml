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

-export([print/1]).

-export([handle_begin/3, handle_end/2, handle_text/2]).

%%--------------------------------------------------------------------
%% @doc Helper function to scan an entire XML document.
%%
%% @end
%%--------------------------------------------------------------------
-spec print(file:filename()) -> ok.
print(File) ->
    {ok, []} = gen_xml:read(File, ?MODULE, []),
    ok.

%%--------------------------------------------------------------------
%% @doc The callback function for begin tags.
%%
%% Each call prints the paths collected so far.
%%
%% The attributes are ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_begin(atom(), list(), list()) -> list().
handle_begin(Tag, _Attr, Tags) ->
    Tags_new = [Tag|Tags],
    Tags_str = [ atom_to_list(A) || A <- lists:reverse(Tags_new) ],
    Path = string:join(Tags_str, "/"),
    io:format("~s~n", [Path]),
    Tags_new.

%%--------------------------------------------------------------------
%% @doc The callback function for end tags.
%%
%% We remove the first (inner tag) from the list.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_end(atom(), list()) -> list().
handle_end(Tag, [Tag|Tags]) ->
    Tags.

%%--------------------------------------------------------------------
%% @doc The callback function for text elements.
%%
%% No action is performed with these elements.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_text(string(), list()) -> list().
handle_text(_Text, Tags) ->
    Tags.

%%--------------------------------------------------------------------
