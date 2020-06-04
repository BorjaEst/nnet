%%%-------------------------------------------------------------------
%%% @doc Module to manage network graph.
%%% @end
%%%-------------------------------------------------------------------
-module(network).

-export([record_fields/0, new/0, copy/1, concat/2, rename/2]).
-export([key/1, info/1, to_map/1, from_map/1]).
-export([no_nnodes/1, nnodes/1, connections/2]).
-export([sink_nnodes/1, bias_nnodes/1]).

-export([add_nnode/2, del_nnode/2]).
-export([in_degree/1, in_nodes/1, out_degree/1, out_nodes/1]).
-export([in_seq/2, out_seq/2]).
-export([in_degree/2, in_nodes/2, out_degree/2, out_nodes/2]).

-export([add_link/3, del_link/2, copy_link/3, move_link/3, 
         seq_path/2]).
-export([in_links/1, out_links/1, links/1, no_links/1]).
-export([in_links/2, out_links/2, links/2, no_links/2]).

-export_type([network/0, connections/0, d_node/0, info/0]).

-type d_node()  :: nnode:id() | 'start' | 'end'.
-type link()    :: seq | rcc.
-record(cn, {
    in  = #{} :: #{d_node() => link()},
    out = #{} :: #{d_node() => link()}
}).
-define( IN(CN), element(#cn.in,  CN)).
-define(OUT(CN), element(#cn.out, CN)).
-record(network, {
    key = make_ref() :: reference(),
    nodes        :: #{d_node() => #{d_node() => #cn{}}},
    inputs  = [] :: [nnode:id()], %Keeps the inputs order
    outputs = [] :: [nnode:id()]  %Keeps the outputs order
}).
-type network() :: #network{}.
-type connections() :: #{in  => #{d_node() => link()},
                         out => #{d_node() => link()}}.
-type info() :: #{'size'        => non_neg_integer(),
                  'connections' => non_neg_integer(),
                  'n_inputs'    => non_neg_integer(),
                  'n_outputs'   => non_neg_integer()}.


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Record fields from network.  
%% @end
%%-------------------------------------------------------------------
-spec record_fields() -> ListOfFields :: [atom()].
record_fields() -> record_info(fields, network).

%%-------------------------------------------------------------------
%% @doc Creates a new network.  
%% @end
%%-------------------------------------------------------------------
-spec new() -> network().
new() -> #network{nodes=#{'start'=>#cn{},'end'=>#cn{}}}.

%%--------------------------------------------------------------------
%% @doc Returns the network id.
%% @end
%%-------------------------------------------------------------------
-spec key(Network :: network()) -> reference().
key(NN) -> NN#network.key.

%%-------------------------------------------------------------------
%% @doc Returns a copy of the network with a different id.
%% @end
%%-------------------------------------------------------------------
-spec copy(Network :: network()) -> network().
copy(NN) -> NN#network{key=make_ref()}.

%%-------------------------------------------------------------------
%% @doc Concatenates networks NN1 and NN2 connecting each output from
%% NN1 to each input of NN2.
%% Note the id of network 3 is different form 1 & 2.
%% @end
%%-------------------------------------------------------------------
-spec concat(NN1, NN2) -> NN3 when
    NN1 :: network(),
    NN2 :: network(),
    NN3 :: network().
concat(NN1, NN2) -> 
    NN1_aux = del_nnode('end', NN1),
    NN2_aux = del_nnode(start, NN2),
    NN3_aux = #network{
        key = make_ref(),
        nodes = maps:merge(NN1_aux#network.nodes, NN2_aux#network.nodes),
        inputs  = in_nodes(NN1),
        outputs = out_nodes(NN2)
    },
    lists:foldl(fun(L,NN) -> add_link(L, seq, NN) end, NN3_aux, 
        [{O1,I2} || O1 <- out_nodes(NN1), I2 <- in_nodes(NN2)]
    ).

%%-------------------------------------------------------------------
%% @doc Renames the map nodes in the network.
%% @end
%%-------------------------------------------------------------------
-spec rename(NN1, NMap) -> NN2 when
    NN1  :: network(),
    NN2  :: network(),
    NMap :: #{Old::d_node() => New::d_node()}.
rename(NN, NMap) -> 
    maps:from_list([{maps:get(N,NMap,N), rename_conn(Conn,NMap)} 
        || {N,Conn} <- maps:to_list(NN#network.nodes)]).
rename_conn(Conn, NMap) -> 
    #cn{
        in  = replace_map( Conn#cn.in, NMap),
        out = replace_map(Conn#cn.out, NMap)
    }.
replace_map(Map, NMap) -> 
    maps:from_list([{maps:get(Node, NMap, Node),Value} 
        || {Node,Value} <- maps:to_list(Map)]).

%%-------------------------------------------------------------------
%% @doc Information from the network.  
%% @end
%%-------------------------------------------------------------------
-spec info(NN :: network()) -> info().
info(#network{} = NN) ->
    #{
        size        => no_nnodes(NN),
        connections => no_links(NN),
        n_inputs    => in_degree(NN),
        n_outpues   => out_degree(NN)
    }.

%%-------------------------------------------------------------------
%% @doc Return a network in a map format. Note this process losses 
%% information such the order of inputs and outputs.
%% @end
%%-------------------------------------------------------------------
-spec to_map(network()) -> #{From::d_node() => [To::d_node()]}.
to_map(NN) ->
    maps:map(fun(_,CN) -> ?OUT(CN) end, NN#network.nodes).

%%-------------------------------------------------------------------
%% @doc Creates a new network from a map.  
%% @end
%%-------------------------------------------------------------------
-spec from_map(#{From => #{To => seq|rcc}}) -> network() when
    From :: d_node(),
    To   :: d_node().
from_map(Map) -> 
    NN = lists:foldl(fun add_nnode/2, new(), maps:keys(Map)),
    map_fun(Map, NN).

map_fun(Map, NN) -> 
    maps:fold(fun map_fun/3, NN, Map).
map_fun(From, Connections, NN) -> 
    maps:fold(map_fun(From), NN, Connections).
map_fun(From) -> 
    fun(To, Type, NN) -> add_link({From,To}, Type, NN) end.

%%-------------------------------------------------------------------
%% @doc Adds a nnode to the network.  
%% @end
%%-------------------------------------------------------------------
-spec add_nnode(N, NN1) -> NN2 when
    N   :: nnode:id(),
    NN1 :: network(),
    NN2 :: network().
add_nnode(N, #network{nodes=Nodes} = NN) ->
    %% TODO: Raise error if already in?
    NN#network{
        nodes = Nodes#{N => #cn{}}
    }.

%%-------------------------------------------------------------------
%% @doc Deletes a nnode from the network.  
%% @end
%%-------------------------------------------------------------------
-spec del_nnode(N, NN1) -> NN2 when
    N   :: nnode:id(),
    NN1 :: network(),
    NN2 :: network().
del_nnode(N, NN1) ->
    NN2 = lists:foldl(fun del_link/2, NN1, links(N, NN1)),
    NN2#network{nodes = maps:remove(N, NN2#network.nodes)}.

%%-------------------------------------------------------------------
%% @doc Returns the node connections.  
%% @end
%%-------------------------------------------------------------------
-spec connections(N, NN) -> connections() when
      NN :: network(),
      N  :: d_node().
connections(N, NN) ->
    CN = map_get(N, NN#network.nodes),
    #{
        in  => ?IN(CN), 
        out => ?OUT(CN)
    }.

%%-------------------------------------------------------------------
%% @doc Returns the number of nnodes of the network.  
%% @end
%%-------------------------------------------------------------------
-spec no_nnodes(NN) -> non_neg_integer() when
      NN :: network().
no_nnodes(NN) ->
    maps:size(NN#network.nodes) - 2. %(-'start' -'end')

%%-------------------------------------------------------------------
%% @doc Returns a list of all nnodes of the network.  
%% @end
%%-------------------------------------------------------------------
-spec nnodes(NN) -> NNodes when
      NN :: network(),
      NNodes :: [nnode:id()].
nnodes(NN) ->
    maps:keys(NN#network.nodes) -- ['start', 'end'].

%%-------------------------------------------------------------------
%% @doc Returns all nnodes in the network without outputs.  
%% @end
%%-------------------------------------------------------------------
-spec sink_nnodes(NN) -> NNodes when
      NN :: network(),
      NNodes :: [nnode:id()].
sink_nnodes(NN) ->
    Pred = fun(_,Conn) -> #{} == Conn#cn.out end,
    Sink = maps:filter(Pred, NN#network.nodes),
    maps:keys(Sink) -- ['start', 'end'].

%%-------------------------------------------------------------------
%% @doc Returns all nnodes in the network without inputs.  
%% @end
%%-------------------------------------------------------------------
-spec bias_nnodes(NN) -> NNodes when
      NN :: network(),
      NNodes :: [nnode:id()].
bias_nnodes(NN) ->
    Pred = fun(_,Conn) -> #{} == Conn#cn.in end,
    Bias = maps:filter(Pred, NN#network.nodes),
    maps:keys(Bias) -- ['start', 'end'].

%%-------------------------------------------------------------------
%% @doc Returns the nnodes connected to start. 
%% @end
%%-------------------------------------------------------------------
-spec in_nodes(NN) -> NNodes when
      NN :: network(),
      NNodes :: [nnode:id()].
in_nodes(NN) ->
    NN#network.inputs.

%%-------------------------------------------------------------------
%% @doc Returns the in-degree of the network.  
%% @end
%%-------------------------------------------------------------------
-spec in_degree(NN) -> non_neg_integer() when
      NN :: network().
in_degree(NN) ->
    out_degree('start', NN).

%%-------------------------------------------------------------------
%% @doc Returns all nodes connected to N inputs in network NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_nodes(N, NN) -> Nodes when
      N  :: d_node(),
      NN :: network(),
      Nodes :: [d_node()].
in_nodes(N, NN) -> 
    maps:keys(?IN(maps:get(N, NN#network.nodes))).

%%-------------------------------------------------------------------
%% @doc Returns all sequential nodes connected to N inputs in 
%% network NN.  
%% @end
%%-------------------------------------------------------------------
-spec in_seq(N, NN) -> Nodes when
      N  :: d_node(),
      NN :: network(),
      Nodes :: [d_node()].
in_seq(N, NN) -> 
    In = ?IN(maps:get(N, NN#network.nodes)),
    [X || {X,seq} <- maps:to_list(In)].

%%-------------------------------------------------------------------
%% @doc Returns the in-degree of the network node.  
%% @end
%%-------------------------------------------------------------------
-spec in_degree(N, NN) -> non_neg_integer() when
      N  :: d_node(),
      NN :: network().
in_degree(N, NN) ->
    maps:size(?IN(maps:get(N, NN#network.nodes))).

%%-------------------------------------------------------------------
%% @doc Returns the nnodes connected to end. 
%% @end
%%-------------------------------------------------------------------
-spec out_nodes(NN) -> NNodes when
      NN :: network(),
      NNodes :: [nnode:id()].
out_nodes(NN) ->
    NN#network.outputs.

%%-------------------------------------------------------------------
%% @doc Returns the out-degree of the network.  
%% @end
%%-------------------------------------------------------------------
-spec out_degree(NN) -> non_neg_integer() when
      NN :: network().
out_degree(NN) ->
    in_degree('end', NN).

%%-------------------------------------------------------------------
%% @doc Returns all nodes connected from N outputs at network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_nodes(N, NN) -> Nodes when
      N  :: d_node(),
      NN :: network(),
      Nodes :: [d_node()].
out_nodes(N, NN) -> 
    maps:keys(?OUT(maps:get(N, NN#network.nodes))).

%%-------------------------------------------------------------------
%% @doc Returns all sequential nodes connected from N outputs at 
%% network NN.  
%% @end
%%-------------------------------------------------------------------
-spec out_seq(N, NN) -> Nodes when
      N  :: d_node(),
      NN :: network(),
      Nodes :: [d_node()].
out_seq(N, NN) -> 
    Out = ?OUT(maps:get(N, NN#network.nodes)),
    [X || {X,seq} <- maps:to_list(Out)].

%%-------------------------------------------------------------------
%% @doc Returns the out-degree of the network node.  
%% @end
%%-------------------------------------------------------------------
-spec out_degree(N, NN) -> non_neg_integer() when
      N  :: d_node(),
      NN :: network().
out_degree(N, NN) ->
    maps:size(?OUT(maps:get(N, NN#network.nodes))).

%%-------------------------------------------------------------------
%% @doc Creates (or modifies) a link between N1 and N2. Atoms 'start'
%% and 'end' can be used as nodes to add a link to the network start
%% or/and end.
%% @end
%%------------------------------------------------------------------
-spec add_link(Link, Type, NN1) -> NN2 when
    Link :: link:link(),
    Type :: seq | rcc,
    NN1  :: network(),
    NN2  :: network().
add_link({start, N2}, seq, NN) ->
    InMap = ?OUT(map_get(start, NN#network.nodes)),
    false = is_map_key(N2, InMap),
    insert_link(seq,NN#network{inputs=[N2|in_nodes(NN)]},start,N2);
add_link({N1, 'end'}, seq, NN) ->
    OutMap = ?IN(map_get('end', NN#network.nodes)),
    false  = is_map_key(N1, OutMap),
    insert_link(seq,NN#network{outputs=[N1|out_nodes(NN)]},N1,'end');
add_link({N1, N2}, Type, NN) ->
    insert_link(Type, NN, N1, N2).

%%-------------------------------------------------------------------
%% @doc Deletes the link between N1 and N2. 
%% @end
%%-------------------------------------------------------------------
-spec del_link(Link, NN1) -> NN2 when
    Link :: link:link(),
    NN1  :: network(),
    NN2  :: network().
del_link({start,   N2 }, NN) -> 
    remove_link(NN#network{ inputs=lists:delete(N2, in_nodes(NN))},start,N2);
del_link({   N1, 'end'}, NN) -> 
    remove_link(NN#network{outputs=lists:delete(N1,out_nodes(NN))},N1,'end');
del_link({   N1,   N2 }, NN) -> 
    remove_link(NN, N1, N2).

%%-------------------------------------------------------------------
%% @doc Copies a link usign a map as #{Old => New}. 
%% @end
%%-------------------------------------------------------------------
-spec copy_link(Link, NN1, NMap) -> NN2 when
    Link  :: link:link(),
    NN1   :: network(),
    NMap  :: #{d_node() => d_node()},
    NN2   :: network().
copy_link({N1, N2}, NN, NMap) -> 
    Type = link_type({N1,N2}, NN),
    N3 = maps:get(N1, NMap, N1),
    N4 = maps:get(N2, NMap, N2),
    add_link({N3,N4}, Type, NN).

%%-------------------------------------------------------------------
%% @doc Moves the link usign a map as #{Old => New}. 
%% @end
%%-------------------------------------------------------------------
-spec move_link(Link, NN1, NMap) -> NN2 when
    Link  :: link:link(),
    NN1   :: network(),
    NMap  :: #{d_node() => d_node()},
    NN2   :: network().
move_link({N1, N2}, NN, NMap) -> 
    del_link({N1,N2}, copy_link({N1, N2}, NN, NMap)).

%%-------------------------------------------------------------------
%% @doc Searches for a path between N1 and N2. 
%% @end
%%-------------------------------------------------------------------
-spec seq_path({From, To}, NN) -> Path when
    From :: d_node(),
    To   :: d_node(),
    NN   :: network(),
    Path :: [d_node()] | not_found.
seq_path({ N, N},  _) -> [N];
seq_path({N1,N2}, NN) ->
    Nexts = out_seq(N1, NN),
    one_path(Nexts, N2, [], [N1], [N1], NN).

%%-------------------------------------------------------------------
%% @doc Returns all links emanating from the start of the network. 
%% @end
%%-------------------------------------------------------------------
-spec in_links(NN) -> Links when
    NN :: network(),
    Links :: [link:link()].
in_links(NN) ->
    out_links(NN, 'start').

%%-------------------------------------------------------------------
%% @doc Returns all links incident on N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_links(N, NN) -> Links when
    N :: d_node(),
    NN :: network(),
    Links :: [link:link()].
in_links(N2, NN) ->
    In = ?IN(maps:get(N2, NN#network.nodes)),
    [{N1,N2} || N1 <- maps:keys(In)].

%%-------------------------------------------------------------------
%% @doc Returns all links incident on the end of the network.  
%% @end
%%-------------------------------------------------------------------
-spec out_links(NN) -> Links when
    NN :: network(),
    Links :: [link:link()].
out_links(NN) ->
    in_links('end', NN).

%%-------------------------------------------------------------------
%% @doc Returns all links emanating from N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_links(N, NN) -> Links when
    N :: d_node(),
    NN :: network(),
    Links :: [link:link()].
out_links(N1, NN) ->
    Out = ?OUT(maps:get(N1, NN#network.nodes)),
    [{N1,N2} || N2 <- maps:keys(Out)].

%%-------------------------------------------------------------------
%% @doc Returs all network links. 
%% @end
%%-------------------------------------------------------------------
-spec links(NN) -> Links when
    NN :: network(),
    Links :: [link:link()].
links(#network{nodes=Nodes} = NN) ->
    lists:append([out_links(N,NN) || N <- maps:keys(Nodes)]).

%%-------------------------------------------------------------------
%% @doc Returs all node links. 
%% @end
%%-------------------------------------------------------------------
-spec links(N, NN) -> Links when
    N :: d_node(),
    NN :: network(),
    Links :: [link:link()].
links(N, NN) ->
    lists:append(in_links(N,NN), out_links(N,NN)).

%%-------------------------------------------------------------------
%% @doc Returs the number of links in the network. 
%% @end
%%-------------------------------------------------------------------
-spec no_links(NN) -> non_neg_integer() when
    NN :: network().
no_links(NN) ->
    length(links(NN)).

%%-------------------------------------------------------------------
%% @doc Returs the number of links in the node. 
%% @end
%%-------------------------------------------------------------------
-spec no_links(N, NN) -> non_neg_integer() when
    N  :: d_node(),
    NN :: network().
no_links(N, NN) ->
    length(links(N, NN)).


%%====================================================================
%% Internal functions
%%====================================================================

% Finds the first path from N1 to N2 --------------------------------
one_path([W| _], W,    _,  _, Ps,  _) -> % The path is found
    lists:reverse([W|Ps]); 
one_path([N|Ns], W, Cont, Xs, Ps, NN) -> 
    case lists:member(N, Xs) of
        true  -> % That nnode were evluated before
            one_path(Ns, W, Cont, Xs, Ps, NN);
        false -> % That nnode out neighbours can be check firts
            Nexts = out_seq(N,NN),
            one_path(Nexts, W, [{Ns,Ps}|Cont], [N|Xs], [N|Ps], NN)
    end;
one_path([], W, [{Ns,Ps}|Cont], Xs, _, NN) -> % End of neighbours
    one_path(Ns, W, Cont, Xs, Ps, NN);
one_path([], _,             [],  _, _,  _) -> % No seq path
    not_found.

% Inserts a link on the nodes ---------------------------------------
insert_link(Type, #network{nodes=Nodes} = NN,  N,  N) ->
    #{N:= Conn} = Nodes,
    NN#network{nodes = Nodes#{
        N := add_out(add_in(Conn, N, Type), N, Type)
    }};
insert_link(Type, #network{nodes=Nodes} = NN, N1, N2) ->
    #{N1:= ConnN1, N2:=ConnN2} = Nodes,
    NN#network{nodes = Nodes#{
        N1 := add_out(ConnN1, N2, Type),
        N2 := add_in( ConnN2, N1, Type)
    }}.

add_out(#cn{out=Out} = ConnN1, N2, Type) -> 
    ConnN1#cn{out = Out#{N2 => Type}}.

add_in(#cn{in=In} = ConnN2, N1, Type) -> 
    ConnN2#cn{in = In#{N1 => Type}}.

% Removes a link from the nodes -------------------------------------
remove_link(#network{nodes=Nodes} = NN,  N,  N) ->
    #{N:= Conn} = Nodes,
    NN#network{nodes = Nodes#{
        N := remove_out(remove_in(Conn, N), N)
    }};
remove_link(#network{nodes=Nodes} = NN, N1, N2) ->
    #{N1:= ConnN1, N2:=ConnN2} = Nodes,
    NN#network{nodes = Nodes#{
        N1 := remove_out(ConnN1, N2),
        N2 := remove_in( ConnN2, N1)
    }}.

remove_out(#cn{out=Out} = ConnN1, N2) -> 
    ConnN1#cn{out = maps:remove(N2, Out)}.

remove_in(#cn{in=In} = ConnN2, N1) -> 
    ConnN2#cn{in = maps:remove(N1, In)}.

% Returns the link type of N1->N2 -----------------------------------
link_type({N1,N2}, NN) -> 
    ConnN1 = map_get(N1, NN#network.nodes),
    map_get(N2, ?OUT(ConnN1)).

