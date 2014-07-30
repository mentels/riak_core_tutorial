riak_core-tutorial
==================

## Multinode Hello World ##

1. Install riak_core rebar_template

    > to może będzie zrobione na maszynce wirtualnej więc na razie nie opisuje
    
2. Generate multinode app  
`./rebar create template=riak_core_multinode appid=sc nodeid=sc`
3. Modify rebar.config to match the relase 1.4.9 and remove:  
   ```
   {deps, [
       {riak_core, "1.4.*",
           {git, "git://github.com/basho/riak_core", {tag, "1.4.9"}}}
   ]}.
   ```

    > At the time of writing this tutorial the latest stable version
    > is 1.4.10. For other version see [releases](https://github.com/basho/riak_core/releases).

4. Generate release with 3 nodes and start them:  
   ```
   make dev rel`  
   for d in dev/dev*; do $d/bin/sc start; done
   ```
5. Check the status of the nodes:  
`./dev/dev1/bin/sc-admin member_status`  
We see that there's only one node in the ring.
6. Join the nodes in a cluster and verify the status again:  
   ```
   for d in dev/dev{2,3}; do $d/bin/sc-admin join sc1@127.0.0.1; done  
   ./dev/dev1/bin/sc-admin member_status  
   ```
   
   Look at the `Ring` column's values. It indicates how much of the key
   space is allocated for a particular node. Over time, each node should
   cover proportional percentage of a ring.
7. Now look how the constant hashing works:
    8. Attach to a node 1:  
       `./dev/dev1/bin/sc attach`  
    9. Run the following snippet to compute the hash on each node:
    ```
    F = fun() ->
        Hashes = [begin
            Node = "sc" ++ integer_to_list(N) ++ "@127.0.0.1",
            rpc:call(list_to_atom(Node), riak_core_util, chash_key, [{<<"ping">>, <<"ping">>}])
        end || N <- [1,2,3]],
        [OneHash] = lists:usort(Hashes)
    end.
    F(), F().
    ```
    10. Note two important things:
        11. Each node returns the same hash for a key.
        12. The hash is the same over time.
13. Now let's see how the objects are dispatch over the ring to vnodes.

    > Jakaś teoria i rysunek obrazujący vnode'a i parametr N.
    >
    > **What is vnode?**
    > A vnode is a virtual node, as opposed to physical node
    > * Each vnode is responsible for one partition on the ring
    > * A vnode is an Erlang process
    > * A vnode is a behavior written atop of the gen_fsm behavior
    > * A vnode handles incoming requests
    > * A vnode potentially stores data to be retrieved later
    > * A vnode is the unit of concurrency, replication, and fault tolerance
    > * Typically many vnodes will run on each physical node
    > 
    > Each machine has a vnode master who's purpose is to keep
    > track of all active vnodes on its node.
    >
    > **What is N?**
    > In a riak_core world it indicates number of vnodes on which we
    > want to perform something. 

    ```
    [Hash] = F().
    riak_core_apl:get_primary_apl(H, _N = 2, sc).
    ```

    `apl` stands for *active preference list*.

## Implementing simple crawler ##

The plan for the next step is to implement a distributed internet
crawler that will be able to download websites and just store them for
later retrieval. So, the desing is as follows:
* downloading will take place on random vnodes in the cluster;
* a particular vnode will store a content of a given URL.

The picture below shows the system for 3 erlang nodes:
> jakiś rysunek tutaj

### Implementing downloader part  ###

We already have an API for downloading in `sc:download/1` so we only
need to implement a vnode that will be handling the actual download
tasks. Actually, a skeleton for the vnode is already there in
`sc_downloader_vnode.erl`. Note, that a vnode have to implement
`riak_core_vnode` behaviour. We're focusing on `hadnle_command/3`
callback, that will be invokded by the
`riak_core_vnode_master:command/3`.

> More information on the `riak_core_vnode` callbacks can be found
> [here](https://github.com/vitormazzi/try-try-try/tree/master/2011/riak-core-the-vnode#life-cycle-callbacks).

So add the asynchronous API:
```
%% API
-export([start_vnode/1,
         download/2]).
...
-define(MASTER, sc_downloader_vnode_master).
...

-spec download({chash:index_as_int(), node()}, binary()) -> term().
download(IdxNode, URL) ->
    riak_core_vnode_master:command(IdxNode, {download, URL}, ?MASTER).
```

The `MASTER` indicates the ID of the master vnode for downlader vonodes.

Next, implement the command:
```
...
handle_command({download, URL} = Req, _Sender, State) ->
    print_request_info(State#state.partition, node(), Req),
    try
        Content = download(URL),
        store(URL, Content)
    catch
        throw:{download_error, Reason} ->
            ?PRINT({request_failed, Req, Reason})
    end,
    {noreply, State};
...
```

In the final step uncomment specification for
`sc_downloader_vnode_master` in `sc_sup.erl` and add it to
the supervisor's child list. Additionaly, register the vnode in
`sc_app.erl`:
```
...
    ok = riak_core:register([{vnode_module, sc_vnode}]),
->  ok = riak_core:register([{vnode_module, sc_downloader_vnode}]),
    ok = riak_core_ring_events:add_guarded_handler(sc_ring_event_handler, []),
....
```

To test our new funcionality stop the whole cluster, clean project,
build devrel again and form the cluster:
```
for d in dev/dev*; do $d/bin/sc stop; done
make devclean && make devrel
for d in dev/dev*; do $d/bin/sc start; done
for d in dev/dev{2,3}; do $d/bin/sc-admin join sc1@127.0.0.1; done
```
Once we have all the setup up and running attach to one of the nodes
and observe the logs in the other two nodes in two separate consoles.
Assuming that you attached to dev1:
```
tail -f dev/dev2/log/erlang.log.1
tail -f dev/dev3/log/erlang.log.1
```

Experiment a bit with `sc:download/1` API:  
`1> sc:download(<<"http://www.erlang.org">>).`  
and note that the reuqests are serverd by a random partitions
on different nodes. Effectively it means request hit different vnodes
(a vnode is responsible for one partition).

> "The randomness" is achieved by picking a vnode for random document
>  index. See `sc:get_random_dument_index/0`.

### Implementing storing requirement ###

Let's move to our storage system. As above is already implemented in
`sc:store/2` and `sc:get_content/2` (just uncomment all the lines in
`sc:store/2`.

> Note, that in this case the same node will be chosen for sotring
> or retireving data for a particular URL.

Similarily as in the previous example we need a vnode to do our job.
There is already such a vnode implemented `sc_storage_vnode.erl`.
Please, have look at its `get_content/3` API function. It invokes
the command using `riak_core_vnode_master:sync_spawn_command/3` that
is asynchronus but doesn't block the vnode. The difference is also
in the command for retrieving the content as we return a reply.

To get it working you also have to take care of the master vnode for
storage in `sc_sup.erl` and register the `sc_storage_vnode.erl` - in
the analogously as with downloader vnode.

When you're done restart the whole machinery using the snippet above.
Next, let's beging testing how it works: so attach to one node and
"tailf" the logs of the others. Download your favourie website 
and get its content:
```
sc:download(<<"http://joemonster.org/">>).
sc:download(<<"http://joemonster.org/">>).
sc:get_content(<<"http://joemonster.org/">>).
```

You would expect that the download request will be served by different
vnodes and each store and get content requests by the same vnode. But,
hey! what if `get_content` return empty list even though the request
match the right partition?! Well, it's possible...

The explanation behind this behaviour is that, when you start your
first node it servers all the partitions which in practice means that
it runs all the vnodes of each kind (by default 64 partitions are
created). When new nodes join the cluster the partitions are spread
across them but it happens in the background - strictly speaking: while
the cluster is serving reuquest it's moving vnodes to other physical
nodes in the same time. But riak_core have no idea how to move our
data so it's just lost! Terrible ha?

To observe the whole system working as expected you need to wait for
the cluster to come in "stable state". Just check the status:  
`./dev/dev1/bin/sc-admin member_status`
When there're now pending changes it means that no partitions will be
moved. Now you can experiment again and make sure, that request are
served by appropriate partitions, vnodes and nodes.

In the next chapter I'm going to explain how to prepare for moving
a vnode: so called *handoff*.

### Notatki ###
1. Erlang R15B03 jest potrzebny
   

