-module(sc).
-include("sc.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         download/1,
         store/2,
         get_content/1
        ]).

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
    IdxNode = get_index_node(DocIdx),
    sc_downloader_vnode:download(IdxNode, URL).

%% doc Store URL's content in a VNode correspoding to the URL
-spec store(string(), binary()) -> term().
store(URL, Content) ->
    DocIdx = get_index_for_url(URL),
    IdxNode = get_index_node(DocIdx),
    sc_storage_vnode:store(IdxNode, {URL, Content}).

-spec get_content(string()) -> {ok, binary()} | not_found.
get_content(URL) ->
    DocIdx = get_index_for_url(URL),
    IdxNode = get_index_node(DocIdx),
    sc_storage_vnode:get_content(IdxNode, URL).

%% Helpers

get_random_document_index() ->
    riak_core_util:chash_key({<<"download">>, term_to_binary(now())}).

get_index_node(DocIdx) ->
    [{IndexNode, _Type}] =
        riak_core_apl:get_primary_apl(DocIdx, 1, sc),
    IndexNode.

get_index_for_url(URL) ->
    riak_core_util:chash_key({<<"url">>, list_to_binary(URL)}).
