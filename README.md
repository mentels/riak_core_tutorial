riak_core_tutorial
==================

## Environment ##

To skip setting up an environment there is already one prepared for this
tutorial: [riak_core_env](https://github.com/mentels/riak_core_env).
In the following chapters I assume that you have the environment
running and do the whole work in `RIAK_CORE_ENV/synced/` directory
mentioned in the link.

## Multinode Hello World ##

> The commands in this chapter has to be invoked from the VM.

#### Installing rebar template for riak_core ####

Folks from Basho where kind enough to prepare a rebar template for
creating riak_core apps. Apart from creating an application structure
it also creates a script for administering the cluster.

Clone the template and install it (under ~/.rebar/templates):
```bash
git clone https://github.com/basho/rebar_riak_core
cd rebar_riak_core && make install
```

> The template provided by Basho is quite old. However there are
> a lot of forks and
> [this one](https://github.com/marianoguerra/rebar_riak_core) seems
> to be adjusted to the newest stable version of riak_core that is
> 1.4.10 at the time of writing this tutorial. For other versions see
> [releases](https://github.com/basho/riak_core/releases).

#### Hello Multinode!!! ####

Once we have the template let's use it to generate an Erlang app. Enter
the `multinode` directory and invoke rebar:
```bash
cd ~/synced/multinode && ./rebar create template=riak_core appid=hwmn nodeid=hwmn
```

Next tweak it a little bit so that we work on the newest stable release
of the beast. Also we need newer lager version. Go and modify freshly
created `rebar.config`:
```bash
{deps, [
    {lager, "2.0.1", {git, "git://github.com/basho/lager", {tag, "2.0.1"}}},
    {riak_core, "1.4.10", {git, "git://github.com/basho/riak_core", {tag, "1.4.10"}}}
]}.
```

We are ready to generate a release with 4 nodes and play with them:
```bash
make devrel
for d in dev/dev*; do $d/bin/hwmn start; done
```

To make sure that we're up and running do:  
`for d in dev/dev*; do $d/bin/hwmn ping; done`  

If you're not getting pongs... well I'm sorry - it worked for me.
But do our nodes know anything about each other? Let's check it using
an admin utility:  
`./dev/dev1/bin/hwmn-admin member_status`  

The output from the above command should like like this:
```bash
================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid     100.0%      --      'hwmn1@127.0.0.1'
-------------------------------------------------------------------------------
```
This simply means that the nodes **ARE NOT** in any relation - node *hwmn1*
knows just about itself. But as you probably already know riak_core
machinery was invented to actually help nodes live together. To join
them, issue:  
`for d in dev/dev{2,3,4}; do $d/bin/hwmn-admin cluster join hwmn1@127.0.0.1; done`  

But this is not enough. We've just **staged** changes to the ring. Before
they take effect we have to **confirm** the plan and **commit**. Yeah,
complicated... but move forward:  
`dev/dev1/bin/hwmn-admin cluster plan`  

We've just get informed by riak_core what will happen. Trust me and
agree by committing:  
`dev/dev1/bin/hwmn-admin cluster commit`  

And check the node's relations again:  
`./dev/dev1/bin/hwmn-admin member_status`  

If your output is similar to the following you managed to make a family:
```bash
================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid      75.0%     25.0%    'hwmn1@127.0.0.1'
valid       9.4%     25.0%    'hwmn2@127.0.0.1'
valid       7.8%     25.0%    'hwmn3@127.0.0.1'
valid       7.8%     25.0%    'hwmn4@127.0.0.1'
-------------------------------------------------------------------------------
Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0
```
Look at the `Ring` column. It indicates how much of the key
space is allocated for a particular node. Over time, each node should
cover proportional percentage of a ring.

#### Consistent hashing ####

What is it? After the
[Riak Glossary](http://docs.basho.com/riak/latest/theory/concepts/glossary/#Consistent-Hashing):
> Consistent hashing is a technique used to limit the reshuffling of
> keys when a hash-table data structure is rebalanced
> (when slots are added or removed). Riak uses consistent hashing
> to organize its data storage and replication.
> Specifically, the vnodes in the Riak Ring responsible for storing
> each object are determined using the consistent hashing technique.

Basically, if we want to perform an operation in a particular riak_core
virtual node (I'll try to explain later mysterious virtual node
- *vnode*) **and** we always want it to be the same vnode for a
particular input, we use consistent hasing. And the resulting hash
value for a given input stays the same regardless of changes to the ring.

As an exercise we can compute a hash value for some input and make
sure that it's the same over the ring. To do so attach to one of
the nodes:  
`./dev/dev1/bin/hwmn attach`  
and run the following snippet:
``` erlang
F = fun() ->
    Hashes = [begin
    Node = "hwmn" ++ integer_to_list(N) ++ "@127.0.0.1",
        rpc:call(list_to_atom(Node), riak_core_util, chash_key, [{<<"please">>, <<"bleed">>}])
    end || N <- [1,2,3]],
    [OneHash] = lists:usort(Hashes),
    OneHash
end.
(Hash = F()) ==  F().
```
> **What is a *vnode*?**
>
> ![alt text](/img/rct_vnode.png)
>
> A *vnode* is a virtual node, as opposed to physical node
> * Each vnode is responsible for one partition on the ring
> * A vnode is an Erlang process
> * A vnode is a behavior written a top of the gen_fsm behavior
> * A vnode handles incoming requests
> * A vnode potentially stores data to be retrieved later
> * A vnode is the unit of concurrency, replication, and fault tolerance
> * Typically many vnodes will run on each physical node
> 
> Each machine has a vnode master who's purpose is to keep
> track of all active vnodes on its node.


How can we make a use of a computed `Hash`? We can get a list of vnodes
on which we can perform/store something.

```
riak_core_apl:get_apl(Hash, _N = 2, hwmn).
```
> `apl` stands for *active preference list*.
>
> The value of `_N` indicates how many vnodes we want to involve in
> performing some operation associated with `Hash`. For example we might
> want to save an object on two vnodes.


The output from the call looks like this:
```erlang
[{1301649895747835411525156804137939564381064921088, 'hwmn2@127.0.0.1'},
 {1324485858831130769622089379649131486563188867072, 'hwmn3@127.0.0.1'}]
```
How to read this? The first element of a tuple is a partition (remember
that a **vnode** is responsible for one partition in the ring?) which is
dedicated to the `Hash` and as you guessed the second element is
a node on which the partition sits!

When you're done with playing with the cluster stop the nodes:  
`for d in dev/dev*; do $d/bin/hwmn stop; done`

Awesome, congratulations, great, sweet just fantastic. Hello Multinode
completed!

## Implementing simple crawler ##

The plan for the next step is to implement a distributed internet
crawler that will be able to download websites and store them for
later retrieval. The desing is as follows:
* downloading will take place on random vnodes in the cluster;
* a particular vnode will store a content of a given URL;
* an old version of a website will be replaced by a new one;
* the API will be implemented in an `sc.erl` module.

This part of the tutorial requires you to implement missing parts of
the `simple_crawler` application that can be found in
`RIAK_CORE_ENV/synced/crawler/simple_crawler`. The
application structure is compliant with Erlang/OTP so all the modules
are in `apps/sc/src`.

### Implementing downloader part  ###

The API for downloader is already implemented in `sc:download/1` so
we only need to add a vnode that will be handling the actual download
tasks. A skeleton for the vnode is already there in
`sc_downloader_vnode.erl`. Note, that a vnode have to implement
`riak_core_vnode` behaviour. Here we're focusing on `handle_command/3`
callback, that will be invokded by the `riak_core_vnode_master:command/3`.

> More information on the `riak_core_vnode` callbacks can be found
> [here](https://github.com/vitormazzi/try-try-try/tree/master/2011/riak-core-the-vnode#life-cycle-callbacks).

Let's get to coding. First of all, add the asynchronous API to the
vnode. Edit the `sc_downloader_vnode.erl`:
```erlang
-export([start_vnode/1,
         download/2]).
...
-define(MASTER, sc_downloader_vnode_master).
...

-spec download({chash:index_as_int(), node()}, binary()) -> term().
download(IdxNode, URL) ->
    riak_core_vnode_master:command(IdxNode, {download, URL}, ?MASTER).
```

`MASTER` indicates the ID of the master vnode for downloader vnodes.

Next, implement the command:
```erlang
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

In the final step provide a specification for 
`sc_downloader_vnode_master` in `sc_sup.erl` and add it to the
supervisor's child list:
```erlang
...
VDownloaderMaster =
    {sc_downloader_vnode_master,
     {riak_core_vnode_master, start_link, [sc_downloader_vnode]},
     permanent, 5000, worker, [riak_core_vnode_master]},
...
{ok, {{one_for_one, 5, 10}, [VMaster, VDownloaderMaster]}}.
```
Additionaly, register the vnode in `sc_app.erl`:
```erlang
...
    ok = riak_core:register([{vnode_module, sc_downloader_vnode}]),
....
```

To test our new funcionality stop the whole cluster, clean project,
build devrel and form the cluster:
```bash
for d in dev/dev*; do $d/bin/sc stop; done
make devclean && make devrel
for d in dev/dev*; do $d/bin/sc start; done
for d in dev/dev{2,3}; do $d/bin/sc-admin join sc1@127.0.0.1; done
```
> This time to make things simpler we won't be **staging** and
> **committing** changes to the riak_core ring.
> [This](https://github.com/rzezeski/rebar_riak_core)
> riak_core rebar template implements such simplified behaviour. And we
> will get by with 3 nodes.

Once we have all the setup up and running attach to one of the nodes
and observe the logs of the other two nodes:
```bash
dev/dev1/bin/sc attach
tail -f dev/dev2/log/erlang.log.1
tail -f dev/dev3/log/erlang.log.1
```

> Run the above commands from separate consoles.

Experiment a bit with `sc:download/1` API:  
`[sc:download("http://www.erlang.org") || _ <- lists:seq(1,10)].`

Note that the reuqests are serverd by random partitions on different
nodes. Effectively it means that requests hit different vnodes (a vnode
is responsible for one partition, right?).

"The randomness" is achieved by picking a vnode for random document
index. See `sc:get_random_dument_index/0` how it works.

### Implementing storage part ###

Let's move to our storage system. As above, the API is already
implemented in `sc:store/2` and `sc:get_content/2` (uncomment
all the lines in these functions). Recall from the design description,
that in this case the same vnode will be chosen for storing or
retrieving data for a particular URL.

Similarly as in the previous example we need a vnode to do our job.
There is already such a vnode implemented `sc_storage_vnode.erl`.
Please, have look at its `get_content/3` API function. It invokes
the command using `riak_core_vnode_master:sync_spawn_command/3` that
is synchronous but **does not** block the vnode. The difference is also
in the command for retrieving the content as we return a reply.

To get it working you have to take care of the master vnode for
storage in `sc_sup.erl` and register the `sc_storage_vnode.erl` in the
`sc_app.erl`- analogously as with downloader vnode.

> The `ID` in the child specification that you need to provide
> in sc_sup.erl (for example sc_storage_vnode_master) must match the
> 3rd argument in the call to `riak_core_vnode_master:command/3`.

When you're done restart the whole machinery, attach to one node and
"tailf" the other nodes' logs:
```bash
for d in dev/dev*; do $d/bin/sc stop; done
make devclean && make devrel
for d in dev/dev*; do $d/bin/sc start; done
for d in dev/dev{2,3}; do $d/bin/sc-admin join sc1@127.0.0.1; done
dev/dev1/bin/sc attach
tail -f dev/dev2/log/erlang.log.1
tail -f dev/dev3/log/erlang.log.1
```

Then download your favorite website and retrieve its content:
```erlang
sc:download("http://joemonster.org/").
sc:download("http://joemonster.org/").
sc:get_content("http://joemonster.org/").
```

You would expect that *download* requests will be served by different
vnodes and each *store* and *get_content* requests by the same vnode.
But, hey! what if `get_content/1` returns nothing but `ok` even though
the request matches the right partition?! Well, it's possible...

The explanation behind this behavior is that, when you start your
first node it servers all the partitions which in practice means that
it runs all the vnodes of each kind (by default 64 partitions are
created). When new nodes join the cluster the partitions are spread
across them but it happens in the background - strictly speaking: while
the cluster is serving a request it's moving vnodes to other physical
nodes in the same time. But riak_core have no idea how to move our
data so it's just lost! Terrible ha?

To observe the whole system working as expected you need to wait for
the cluster to come into "stable state". Just check the status:  
`./dev/dev1/bin/sc-admin member_status`

When there're no pending changes it means that no partitions will be
moved. Now you can experiment again and make sure, that requests are
served by appropriate partitions, vnodes and nodes.

In the next part I'm going to explain how not to lose data while moving
vnode to another erlang node: so called *handoff*.

## Handoff ##

### What is handoff?  ###

A *handoff* occurs when a vnode realizes that it's not on the proper
Erlang node. Such a situation can take place when:
* a node is added to or removed from the ring,
* a node comes alive after it has been down.

In riak_core there's a periodic "home check" that verifies whether
a vnode uses correct physical node. If that's not true for some vnode
it will go into *handoff mode* and data will be transferred. 

### Handling handoff ###

When riak_core decides to perform a handoff it calls two functions:
`Mod:handoff_starting/2` and `Mod:is_empty/1`. Through the first one
a vnode can agree on or not to proceede with the handoff. The second one
indicates if there's any data to be transfered and this one is
interesting for us. The vnode process started in `sc_storage_vnode` saves
all the webpages' content into a dict data structure. Thus when a handoff
occurs we want this dict to be transferred.

So lets code `sc_storage_vnode:is_empty/1`:
```erlang
is_empty(State) ->
    case dict:size(State#state.store) of
        0 ->
            {true, State};
        _ ->
            {false, State}
    end.
```

When the framework decides to start handoff it sends `?FOLD_REQ` that
that says the vnode how to fold over its data. This request is supposed
to be handled in `Mod:handle_handoff_command/3` and contains "folding
function" along with an initial accumulator. We should implement
`handle_handoff_command/3` as follows:
```erlang
handle_handoff_command(?FOLD_REQ{foldfun = Fun, acc0=Acc0},
                       _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, State#state.store),
    {reply, Acc, State}.
```

> If you're like my and this ?FOLD_REQ looks strange to you have a look
> at the
> [riak_core_vnode.hrl](https://github.com/basho/riak_core/blob/master/include/riak_core_vnode.hrl)
> that reveals that the macro expands to a record.

So, what's next with this magic handoff? Well, at this point things
are simple: each iteration of "folding function" calls
`Mod:encode_handoff_item/2` that just do what it is supposed to do:
encode data before sending it to the targe vnode. The target vnode,
decodes the data in `Mod:handle_handoff_data/2`. In this tutorial we are
using *extremely complex method of encoding* so write the following code
in your storage vnode really carefully:
```erlang
encode_handoff_item(URL, Content) ->
    term_to_binary({URL, Content}).
...
handle_handoff_data(Data, State) ->
    {URL, Content} = binary_to_term(Data),
    Dict = dict:store(URL, Content, State#state.store),
    {reply, ok, State#state{store = Dict}}.
```

And that's it. Our system is now "move-vnode-resistant". One more thing
worth noting here: after the handoff is completed `Mod:delete/1`
is called and the vnode will be terminated just after this call.
During the termination `Mod:terminate/2` will be called too.

> I did not present all the callbacks related to handoff. For more
> information go to a great tutorial
[here](https://github.com/vitormazzi/try-try-try/tree/master/2011/riak-core-the-vnode#handoff).
> If you need more details look at the
[basho wiki](https://github.com/basho/riak_core/wiki/Handoffs).

### See handoff in action ###

Now, when we have handoff implemented, build devrel, start the cluster,
**but only join dev2 to dev1**. We want to observe how the partitions
are moved:
```bash
for d in dev/dev*; do $d/bin/sc stop; done
make devclean && make devrel
for d in dev/dev*; do $d/bin/sc start; done
dev/dev2/bin/sc-admin join sc1@127.0.0.1
```
Wait for the ring to get populated symmetrically across the two nodes.
Use `./dev/dev1/bin/sc-admin member_status` to check the status.

Next attach to the console of one node from the cluster "tailf"
logs of the other node:
```bash
dev/dev1/bin/sc attach
tail -f dev/dev2/log/erlang.log.1
```

Download some websites using `sc:get_links/0`. This functions will return
a list of 50 URLS:
```erlang
[begin sc:download(L), timer:sleep(500) end  || L <- sc:get_links()].
```

After that join the 3rd node and "tailf" its log. Wait for the cluster
to get balanced and try to retrieve previously downloaded content using
the attached console:
```bash
dev/dev3/bin/sc-admin join sc1@127.0.0.1
tail -f dev/dev3/log/erlang.log.1
```
```erlang
spawn(fun() -> [begin
    sc:get_content(L),
    timer:sleep(500)
    end || L <- sc:get_links()] end).
```

You should see that some URL's content is served by the 3rd node,
although it joined the cluster after all the sites had been downloaded.

## Fault tolerance (in progress) ##

Without destroying the previous setup stop one of the nodes that you know
holds content for some website. Then try to get content of the website.
You should end up with `not_found` response:
```erlang
(sc1@127.0.0.1)9> sc:get_content("http://en.wikipedia.org/").
not_found
```

To tackle this problem we need to store our data on more that one vnode.
Lets code it in via by changing  `sc:get_index_node/1` so that it gets
number of vnodes:
```erlang
get_index_node(DocIdx, N) ->
    riak_core_apl:get_apl(DocIdx, N, sc),
```

It also requires us to repair `sc:download/1`, `sc:store/2` and
`sc:get_content/1`. Let's say that we want to store data on 3 vnodes:
```erlang
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
                    (X) ->
                         X
                      end, R0),
    case R1 of
        [] ->
            not_found;
        _ ->
            hd(R1)
    end.
```
   

