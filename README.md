riak_core-tutorial
==================

### Multinode Hello World ###

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
        [OneHash, _] = lists:usort(Hashes)
    end.
    F(), F().
    ```
    10. Note two important things:
        11. Each node returns the same hash for a key.
        12. The hash is the same over time.
13. Now let's see how the objects are dispatch over the ring to vnodes.

    > Jakaś teoria i rysunek obrazujący vnode'a i parametr N.
    >
    > What is vnode?
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

    ```
    [Hash] = F().
    riak_core_apl:get_primary_apl(H, _N = 2, sc).
    ```

    `apl` stands for *active preference list*.

### Notatki ###
1. Erlang R15B03 jest potrzebny
   

