-module(sc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case sc_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, sc_vnode}]),
            ok = riak_core:register([{vnode_module, sc_downloader_vnode}]),
            ok = riak_core:register([{vnode_module, sc_storage_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(sc_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(sc_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(sc, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
