-module(sc).
-include("sc.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0,
         download/1,
         store/2,
         get_content/1,
         get_links/0]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, sc),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, sc_vnode_master).

%% @doc Dispatch downloading URL's content to a random download_vnode.
-spec download(string()) -> term().
download(URL) ->
    DocIdx = get_random_document_index(),
    IdxNodes = get_index_node(DocIdx, 1),
    sc_downloader_vnode:download(IdxNodes, URL).

%% @doc Store URL's content in a VNode correspoding to the URL
-spec store(string(), binary()) -> term().
store(URL, Content) ->
    DocIdx = get_index_for_url(URL),
    IdxNodes = get_index_node(DocIdx, 3),
    sc_storage_vnode:store(IdxNodes, {URL, Content}).

%% @doc Get content for a given URL.
-spec get_content(string()) -> {ok, binary()} | not_found.
get_content(URL) ->
    DocIdx = get_index_for_url(URL),
    IdxNodes = get_index_node(DocIdx, 3),
    R0 = [sc_storage_vnode:get_content(IN, URL) || IN <- IdxNodes],
    R1 = lists:filter(fun(not_found) ->
                              false;
                         (_) ->
                              true
                      end, R0),
    case R1 of
        [] ->
            not_found;
        _ ->
            hd(R1)
    end.

%% @doc Get URL from ../../links.txt
get_links() ->
    {ok, File} = file:open("../../links.txt", [read]),
    get_links(File, []).

%% Helpers

get_random_document_index() ->
    riak_core_util:chash_key({<<"download">>, term_to_binary(now())}).

get_index_node(DocIdx, N) ->
    riak_core_apl:get_apl(DocIdx, N, sc).

get_index_for_url(URL) ->
    riak_core_util:chash_key({<<"url">>, list_to_binary(URL)}).

get_links(File, Acc) ->
    case io:get_line(File, "") of
        eof ->
            file:close(File),
            Acc;
        URL ->
            get_links(File, [strip_new_line(URL) | Acc])
    end.

strip_new_line(URL) ->
    case re:replace(URL, "\n", "") of
        [U, _] ->
            binary_to_list(U);
        URL ->
            URL
    end.
                
