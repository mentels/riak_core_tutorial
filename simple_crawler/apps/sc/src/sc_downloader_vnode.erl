-module(sc_downloader_vnode).
-behaviour(riak_core_vnode).
-include("sc.hrl").

%% API
-export([start_vnode/1,
        download/2]).

%% Behaviour API
-export([init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-record(state, {partition}).
-define(MASTER, sc_downloader_vnode_master).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

-spec download({chash:index_as_int(), node()}, binary()) -> term().
download(IdxNode, URL) ->
    riak_core_vnode_master:command(IdxNode, {download, URL}, ?MASTER).

%% Callbacks

init([Partition]) ->
    {ok, #state {partition = Partition}}.

handle_command({download, URL} = Req, _Sender, State) ->
    print_request_info(State#state.partition, node(), Req),
    try
        Content = download(URL),
        store(URL, Content)
    catch
        _:Reason ->
            ?PRINT({request_failed, Req, Reason})
    end,
    {noreply, State};
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Herlpers

print_request_info(Partition, Node, Request) ->
    io:format("~n"
              "Request: ~p~n"
              "Partition: ~p~n"
              "Node: ~p~n",
              [Request, Partition, Node]).

download(URL) ->
    case httpc:request(URL) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            Body;
        {error, Reason} ->
            throw({download_error, Reason});
        {ok, {{_Version, _, ReasonPhrase}, _Headers, _Body}} ->
            throw({download_error, ReasonPhrase});
        Other ->
            throw({download_error, Other})
    end.

store(URL, Content) ->
    sc:store(URL, Content).
