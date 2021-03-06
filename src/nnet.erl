%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(nnet).
-compile({no_auto_import,[nodes/1]}).

%% API
-export([start_tables/0, info/1, size/1, nodes/1, all_networks/0]).
-export([from_model/1, compile/1, to_map/1, to_map/2, clone/1, delete/1]).
%% NNode operations (run inside 'fun edit/1') 
-export([rnode/1, wnode/2, rlink/1, wlink/2]).
-export([out/1, out_seq/1, out_rcc/1, in/1, lx/1]).
%% Connections operations (run inside 'fun edit/1')
-export([connect/1, connect_seq/1, connect_rcc/1, disconnect/1]).
-export([move/2, reset/1]).
%% Network operations (run inside 'fun edit/1'
-export([copy/2, clone/2, divide/2, split/2, delete/2, join/2]).
%% Exported types
-export_type([id/0, nnode/0, link/0, info/0, model/0]).

-type id()        :: network:id().
-type network()   :: network:id().
-type nnode()     :: nnode:id().
-type nany()      :: nnode() | network().
-type link()      :: {From::nany(), To::nany()}.
-type model()     :: model:model().
-type info()      :: #{size    => integer(),  % Network size
                       seq     => integer(),  % Sequential outputs
                       rcc     => integer(),  % Reccurrent outputs
                       in_conn => integer(),  % Input connections
                       inputs  => integer(),  % Number of inputs
                       outputs => integer()}. % Number of outputs


%%%===================================================================
%%% Main API functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Creates a new network and returns its id.
%% @end
%%-------------------------------------------------------------------
-spec start_tables() -> ok.
start_tables() ->
    true = new_table( network, network:table_configuration()       ),
    true = new_table( link_in,    link:table_in_configuration()    ),
    true = new_table(link_seq,    link:table_seq_configuration()   ),
    true = new_table(link_rcc,    link:table_rcc_configuration()   ),
    true = new_table(  weight,    link:table_weight_configuration()),
    true = new_table(   nnode,   nnode:table_configuration()       ),
    ok.

%%-------------------------------------------------------------------
%% @doc Creates a new network from a model and returns its id.
%% Layers "input" and "output" are mandatory.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec from_model(Model::model()) -> Id::id().
from_model(Model) -> 
    model:compile(Model).

%%-------------------------------------------------------------------
%% @doc See "from_model/1"
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec compile(Model::model()) -> Id::id().
compile(Model) -> 
    model:compile(Model).

%%-------------------------------------------------------------------
%% @doc Clones a network.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec clone(Id::id()) -> Clone::id().
clone(Id) -> 
    NNodes = maps:keys(network:nnodes(Id)),
    Links  = lists:usort(lists:append([lx(N) || N <- NNodes])),
    NMap   = map_clone(NNodes),
    Clone  = network:clone(Id),
    ok = network:rename(Clone, NMap),
    F = fun(L) -> ok = link:clone(L, NMap#{Id=>Clone}) end,
    ok = lists:foreach(F, Links),
    Clone.

%%-------------------------------------------------------------------
%% @doc Deletes a network.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec delete(Id::id()) -> ok.
delete(Id) -> 
    Nodes = maps:keys(network:nnodes(Id)),
    ok = lists:foreach(fun(N) -> ok = delete(N,Id) end, Nodes),
    Links = lx(Id),
    ok = lists:foreach(fun(L) -> ok = link:del(L) end, Links),
    ok = network:delete(Id).

%%-------------------------------------------------------------------
%% @doc Returns a map of all nnodes of the network.  
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec nodes(Id::id()) -> NNodes::#{nnode:id() => nnode}.
nodes(Id) -> network:nnodes(Id).

%%-------------------------------------------------------------------
%% @doc Returns a map of all nnodes of the network.  
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec size(Id::id()) -> Size::integer().
size(Id) -> map_size(nodes(Id)).

%%-------------------------------------------------------------------
%% @doc Returns network information.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec info(Id::id()) -> Info::info().
info(Id) ->
    NNodes = nodes(Id),
    LNodes = maps:keys(NNodes),
    #{
        size    => map_size(NNodes),
        out_seq => lists:sum([length(link:seq(N)) || N <- LNodes]),
        out_rcc => lists:sum([length(link:rcc(N)) || N <- LNodes]),
        in_conn => lists:sum([length(link:in(N))  || N <- LNodes]),
        inputs  => length(out(Id)),
        outputs => length(in(Id))
    }.

%%-------------------------------------------------------------------
%% @doc Returns network connections in a map format.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec to_map(Id::id()) -> Map::term().
to_map(Id) -> to_map(Id, out_seq).

-spec to_map(Id::id(), Item::atom()) -> Map::term().
to_map(Id,      in) ->  map_conn(fun(X,_) ->  in(X)     end, Id);
to_map(Id,     out) ->  map_conn(fun(X,_) -> out(X)     end, Id);
to_map(Id, out_seq) ->  map_conn(fun(X,_) -> out_seq(X) end, Id);
to_map(Id, out_rcc) ->  map_conn(fun(X,_) -> out_rcc(X) end, Id).

map_conn(Function, Id) -> 
    NNodes = nodes(Id),
    maps:map(Function, NNodes#{Id => network}).

%%-------------------------------------------------------------------
%% @doc Returns a list with all networks.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec all_networks() -> Ids::[id()].
all_networks() -> 
    mnesia:all_keys(network).


%%%===================================================================
%%% API: NNode modifications
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Reads a nnode.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec rnode(NNode) -> Data when 
    NNode :: nnode(),
    Data  :: #{term() => term()}.
rnode(NNode) -> 
    nnode:read(NNode).

%%-------------------------------------------------------------------
%% @doc Edits a nnode.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec wnode(NNode, Data) -> ok when 
    NNode :: nnode(),
    Data  :: #{term() => term()}.
wnode(NNode, Data) -> 
    nnode:edit(NNode, Data).

%%-------------------------------------------------------------------
%% @doc Reads a link.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec rlink(Link) -> Weight when 
    Link   :: nnode(),
    Weight :: number() | not_init.
rlink(Link) -> 
    link:read(Link).

%%-------------------------------------------------------------------
%% @doc Edits a link.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec wlink(Link, Weight) -> ok when 
    Link   :: nnode(),
    Weight :: number() | not_init.
wlink(Link, Weight) -> 
    link:write(Link, Weight).

%%-------------------------------------------------------------------
%% @doc Returns the out links.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec out(From::nany()) -> Out::[{From::nany(), To::nany()}].
out(NNode) -> lists:usort(link:seq(NNode) ++ link:rcc(NNode)).

-spec out_seq(From::nany()) -> Out::[{From::nany(), To::nany()}].
out_seq(NNode) -> link:seq(NNode).

-spec out_rcc(From::nany()) -> Out::[{From::nany(), To::nany()}].
out_rcc(NNode) -> link:rcc(NNode).

%%-------------------------------------------------------------------
%% @doc Returns the in links.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec in(To::nany()) -> In::[{From::nany(), To::nany()}].
in(NNode) -> link:in(NNode).

%%-------------------------------------------------------------------
%% @doc Returns all the nnode links.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec lx(nany()) -> Links::[{From::nany(), To::nany()}].
lx(NNode) ->
    lists:usort(
        lists:append([link:seq(NNode),
                      link:rcc(NNode),
                      link:in(NNode)])
    ).


%%%===================================================================
%%% API: Connections modifications 
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Adds links selecting automatically the nature.
%% By default it implements a sequential, but creates recurrent in 
%% case a dead lock is detected.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec connect(Links::[link()]) -> ok.
connect(Links) -> 
    Add_AllowedLink = fun(L) -> ok = add_allowed_link(L) end,
    lists:foreach(Add_AllowedLink, Links).

add_allowed_link({N1,N2}) ->
    case link:seq_path({N2,N1}) of 
        false -> link:add({N1,N2}, seq, not_init);
        _Path -> link:add({N1,N2}, rcc, not_init)
    end.

%%-------------------------------------------------------------------
%% @doc Adds links as sequential to the network.
%% If not used correctly might create dead locks, use connect/2 if
%% you are not sure if a link would create a dead lock.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec connect_seq(Links::[link()]) -> ok.
connect_seq(Links) -> 
    Add_SeqLink = fun(L) -> link:add(L, seq, not_init) end,
    ok = lists:foreach(Add_SeqLink, Links).

%%-------------------------------------------------------------------
%% @doc Adds links as recurrent.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec connect_rcc(Links::[link()]) -> ok.
connect_rcc(Links) -> 
    Add_RccLink = fun(L) -> link:add(L, rcc, not_init) end,
    ok = lists:foreach(Add_RccLink, Links).

%%-------------------------------------------------------------------
%% @doc Removes links. If the link did not existed nothing happens.
%% Note it might break the network.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec disconnect(Links::[link()]) -> ok.
disconnect(Links) -> 
    Del_Link = fun(L) -> link:del(L) end,
    ok = lists:foreach(Del_Link, Links).

%%-------------------------------------------------------------------
%% @doc Moves the links using a map (Weights are merged).
%% Note it might break the network.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec move(Links::[link()], #{Old::nnode() => New::nnode()}) -> ok.
move(Links, NMap) -> 
    Move_Link = fun(L) -> link:move(L,NMap) end,
    ok = lists:foreach(Move_Link, Links).

%%-------------------------------------------------------------------
%% @doc Reinitialises the weights of the input links. 
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec reset(Links::[link()]) -> ok. 
reset(Links) -> 
    Reset_Link = fun(L) -> link:write(L,not_init) end,
    ok = lists:foreach(Reset_Link, Links).


%%%===================================================================
%%% API: Network modifications
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Copies the nnode and connections, but not the weights.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec copy(NNode1::nnode(), NNet::id()) -> NNode2::nnode().
copy(NNode1, NNet) -> 
    #{NNode1 := NNode2} = NMap = map_clone([NNode1]),
    Copy_link = fun(L) -> ok = link:copy(L, NMap) end,
    lists:foreach(Copy_link, lx(NNode1)),
    network:add_nnode(NNode2, NNet),
    NNode2.

%%-------------------------------------------------------------------
%% @doc Copies the nnode, together with the link values.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec clone(NNode1::nnode(), NNet::id()) -> NNode2::nnode().
clone(NNode1, NNet) -> 
    #{NNode1 := NNode2} = NMap = map_clone([NNode1]),
    Clone_link = fun(L) -> ok = link:clone(L, NMap) end,
    lists:foreach(Clone_link, lx(NNode1)),
    network:add_nnode(NNode2, NNet),
    NNode2.

%%-------------------------------------------------------------------
%% @doc Clones the nnode, but new and old links are redeuced 50%. 
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec divide(NNode1::nnode(), NNet::id()) -> NNode2::nnode().
divide(NNode1, NNet) -> 
    #{NNode1 := NNode2} = NMap = map_clone([NNode1]),
    Divide_link = fun(L) -> ok = link:divide(L, NMap) end,
    lists:foreach(Divide_link, lx(NNode1)),
    network:add_nnode(NNode2, NNet),
    NNode2.

%%-------------------------------------------------------------------
%% @doc Copies the nnode and distributes links (equal probability). 
%% The original nnode must have at least 2 inputs and 2 outputs. 
%% The result nnode will have at least 1 input and 1 output.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec split(NNode1::nnode(), NNet::id()) -> NNode2::nnode().
split(NNode1, NNet) -> 
    #{NNode1 := NNode2} = NMap = map_clone([NNode1]),
    In_move  = random_split( in(NNode1)),
    Out_move = random_split(out(NNode1)),
    move(In_move++Out_move, NMap),
    network:add_nnode(NNode2, NNet),
    NNode2.

%%-------------------------------------------------------------------
%% @doc Deletes a nnode together with all its connections and links.
%% Should run inside a mnesia transaction.
%% @end
%%------------------------------------------------------------------- 
-spec delete(NNode::nnode(), NNet::id()) -> ok.
delete(NNode, NNet) -> 
    Delete_link = fun(L) -> ok = link:del(L) end,
    lists:foreach(Delete_link, in(NNode)++out(NNode)),
    network:del_nnode(NNode, NNet),
    nnode:delete(NNode).

%%-------------------------------------------------------------------
%% @doc Merges all inputs and outputs of all nnodes from left to 
%% right. NNode1 is deleted after the operation.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec join({NNode1::nnode(), NNode2::nnode()}, NNet::id()) -> ok.
join({NNode1, NNode1},    _) -> ok;
join({NNode1, NNode2}, NNet) -> 
    move(lx(NNode1), #{NNode1 => NNode2}),
    delete(NNode1, NNet).


%%====================================================================
%% Internal functions
%%====================================================================

% Creates a new table -----------------------------------------------
new_table(Name, Configuration) ->
    case mnesia:create_table(Name, Configuration) of
        {atomic, ok} -> true;
        {aborted, {already_exists, Name}} -> check(Name, Configuration);
        Other -> Other
    end.

% Checks the table has the correct attributes -----------------------
-define(BAD_TABLE, "table ~s exists using invalid configuration ~p").
check(Name, [{Key,Val}|Configuration]) ->
    case mnesia:table_info(Name, Key) of 
        Val -> check(Name, Configuration);
        _   -> exit(io_lib:format(?BAD_TABLE, [Name, {Key,Val}]))
    end;
check(_Name, []) -> true.

% Copies the nnodes and retruns a map #{Old=>New} ------------------
map_clone(NNodes)       -> map_clone(NNodes, #{}).
map_clone([N|Nx], NMap) -> map_clone(Nx, NMap#{N => nnode:clone(N)});
map_clone(    [], NMap) -> NMap. 

% Returns a non empty/full list of random elements ------------------
random_split([A,B]) -> 
    case rand:uniform() of 
        X when X < 0.5 -> [A];
        _              -> [B]
    end;
random_split([_,_|_] = Links) -> 
    case ltools:rand(Links, 0.5) of 
        []    -> random_split(Links); 
        Links -> random_split(Links); 
        Other -> _Result = Other           
    end;
random_split(_Other) -> 
    error({badarg, "Request a list of at least 2 inputs/outputs"}).


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

