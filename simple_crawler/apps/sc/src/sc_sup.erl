-module(sc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { sc_vnode_master,
                  {riak_core_vnode_master, start_link, [sc_vnode]},
                permanent, 5000, worker, [riak_core_vnode_master]},

    VDownloaderMaster =
        {sc_downloader_vnode_master,
         {riak_core_vnode_master, start_link, [sc_downloader_vnode]},
         permanent, 5000, worker, [riak_core_vnode_master]},

    VStorageMaster =
        {sc_storage_vnode_master,
         {riak_core_vnode_master, start_link, [sc_storage_vnode]},
         permanent, 5000, worker, [riak_core_vnode_master]},

    { ok,
        { {one_for_one, 5, 10},
          [VMaster, VDownloaderMaster, VStorageMaster]}}.
