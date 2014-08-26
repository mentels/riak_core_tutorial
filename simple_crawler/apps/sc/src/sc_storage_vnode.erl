-module(sc_storage_vnode).
-behaviour(riak_core_vnode).
-include("sc.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

%% API
-export([start_vnode/1,
         store/2,
         get_content/2]).

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

-record(state, {partition,
                store :: dict()}).

-define(MASTER, sc_storage_vnode_master).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

-spec store({chash:index_as_int(), node()}, {string(), binary()}) -> term().
store(IdxNode, {URL, Content}) ->
    riak_core_vnode_master:command(IdxNode, {store, URL, Content},
                                   ?MASTER).

get_content(IdxNode, URL) ->
    riak_core_vnode_master:sync_spawn_command(IdxNode,
                                              {get_content, URL},
                                              ?MASTER).
%% Callbacks

init([Partition]) ->
    {ok, #state {partition = Partition, store = dict:new()}}.

handle_command({store, URL, Content}, _Sender, State0) ->
    print_request_info(State0#state.partition, node(), {store, URL}),
    State1 = do_store(URL, Content, State0),
    {noreply, State1};
handle_command({get_content, URL} = Req, _Sender, State) ->
    print_request_info(State#state.partition, node(), Req),
    Reply = case do_get_content(URL, State) of
                not_found = NF ->
                    NF;
                Content ->
                    {ok, Content}
            end,
    {reply, Reply, State};
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun = Fun, acc0=Acc0},
                       _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, State#state.store),
    {reply, Acc, State};
handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(Data, State) ->
    {URL, Content} = binary_to_term(Data),
    Dict = dict:store(URL, Content, State#state.store),
    {reply, ok, State#state{store = Dict}}.

encode_handoff_item(URL, Content) ->
    term_to_binary({URL, Content}).

is_empty(State) ->
    case dict:size(State#state.store) of
        0 ->
            {true, State};
        _ ->
            {false, State}
    end.

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

do_store(URL, Content, #state{store = Dict} = State) ->
    State#state{store = dict:store(URL, Content, Dict)}.

do_get_content(URL, #state{store = Dict}) ->
    case dict:find(URL, Dict) of
        error ->
            not_found;
        {ok, Content} ->
            Content
    end.
