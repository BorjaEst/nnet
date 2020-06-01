%%%-------------------------------------------------------------------
%%% @doc Module to manage network graph.
%%% @end
%%%-------------------------------------------------------------------
-module(network).

-export([record_fields/0, new/0, copy/1, concat/2, rename/2]).
-export([key/1, info/1, to_map/1, from_map/1]).
-export([no_neurons/1, neurons/1, connections/2]).
-export([sink_neurons/1, bias_neurons/1]).

-export([add_neuron/2, del_neuron/2]).
-export([in_degree/1, in_nodes/1, out_degree/1, out_nodes/1]).
-export([in_degree/2, in_nodes/2, out_degree/2, out_nodes/2]).

-export([add_link/2, del_link/2, move_link/3, path/2]).
-export([in_links/1, out_links/1, links/1, no_links/1]).
-export([in_links/2, out_links/2, links/2, no_links/2]).

-export_type([network/0, connections/0, d_node/0, info/0]).

-type d_node()  :: neuron:id() | 'start' | 'end'.
-record(cn, {
    in  = #{} :: #{d_node() => link},
    out = #{} :: #{d_node() => link}
}).
-define( IN(CN), element(#cn.in,  CN)).
-define(OUT(CN), element(#cn.out, CN)).
-record(network, {
    key = make_ref() :: reference(),
    nodes        :: #{d_node() => #{d_node() => #cn{}}},
    inputs  = [] :: [neuron:id()], %Keeps the inputs order
    outputs = [] :: [neuron:id()]  %Keeps the outputs order
}).
-type network() :: #network{}.
-type connections() :: #{in  => #{d_node() => link},
                         out => #{d_node() => link}}.
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
    NN1_aux = del_neuron('end', NN1),
    NN2_aux = del_neuron(start, NN2),
    NN3_aux = #network{
        key = make_ref(),
        nodes = maps:merge(NN1_aux#network.nodes, NN2_aux#network.nodes),
        inputs  = in_nodes(NN1),
        outputs = out_nodes(NN2)
    },
    lists:foldl(fun add_link/2, NN3_aux, 
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
        size        => no_neurons(NN),
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
    maps:map(fun(_,CN) -> maps:keys(?OUT(CN)) end, NN#network.nodes).

%%-------------------------------------------------------------------
%% @doc Creates a new network from a map.  
%% @end
%%-------------------------------------------------------------------
-spec from_map(#{From::d_node() => [To::d_node()]}) -> network().
from_map(Map) -> 
    NN = lists:foldl(fun add_neuron/2, new(), maps:keys(Map)),
    lists:foldl(fun add_link/2, NN, 
        [{From,To} || From <- maps:keys(Map), To <- map_get(From, Map)]
    ).

%%-------------------------------------------------------------------
%% @doc Adds a neuron to the network.  
%% @end
%%-------------------------------------------------------------------
-spec add_neuron(N, NN1) -> NN2 when
    N   :: neuron:id(),
    NN1 :: network(),
    NN2 :: network().
add_neuron(N, #network{nodes=Nodes} = NN) ->
    %% TODO: Raise error if already in?
    NN#network{
        nodes = Nodes#{N => #cn{}}
    }.

%%-------------------------------------------------------------------
%% @doc Deletes a neuron from the network.  
%% @end
%%-------------------------------------------------------------------
-spec del_neuron(N, NN1) -> NN2 when
    N   :: neuron:id(),
    NN1 :: network(),
    NN2 :: network().
del_neuron(N, NN1) ->
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
%% @doc Returns the number of neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec no_neurons(NN) -> non_neg_integer() when
      NN :: network().
no_neurons(NN) ->
    maps:size(NN#network.nodes) - 2. %(-'start' -'end')

%%-------------------------------------------------------------------
%% @doc Returns a list of all neurons of the network.  
%% @end
%%-------------------------------------------------------------------
-spec neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
neurons(NN) ->
    maps:keys(NN#network.nodes) -- ['start', 'end'].

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network without outputs.  
%% @end
%%-------------------------------------------------------------------
-spec sink_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
sink_neurons(NN) ->
    Pred = fun(_,Conn) -> #{} == Conn#cn.out end,
    Sink = maps:filter(Pred, NN#network.nodes),
    maps:keys(Sink) -- ['start', 'end'].

%%-------------------------------------------------------------------
%% @doc Returns all neurons in the network without inputs.  
%% @end
%%-------------------------------------------------------------------
-spec bias_neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
bias_neurons(NN) ->
    Pred = fun(_,Conn) -> #{} == Conn#cn.in end,
    Bias = maps:filter(Pred, NN#network.nodes),
    maps:keys(Bias) -- ['start', 'end'].

%%-------------------------------------------------------------------
%% @doc Returns the neurons connected to start. 
%% @end
%%-------------------------------------------------------------------
-spec in_nodes(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
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
%% @doc Returns all nodes emanating from N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec in_nodes(N, NN) -> Nodes when
      N  :: d_node(),
      NN :: network(),
      Nodes :: [d_node()].
in_nodes(N, NN) -> 
    maps:keys(?IN(maps:get(N, NN#network.nodes))).

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
%% @doc Returns the neurons connected to end. 
%% @end
%%-------------------------------------------------------------------
-spec out_nodes(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron:id()].
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
%% @doc Returns all nodes incident in N of network NN. 
%% @end
%%-------------------------------------------------------------------
-spec out_nodes(N, NN) -> Nodes when
      N  :: d_node(),
      NN :: network(),
      Nodes :: [d_node()].
out_nodes(N, NN) -> 
    maps:keys(?OUT(maps:get(N, NN#network.nodes))).

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
-spec add_link(Link, NN1) -> NN2 when
    Link :: link:link(),
    NN1 :: network(),
    NN2 :: network().
add_link({start, N2}, NN) ->
    InMap = ?OUT(map_get(start, NN#network.nodes)),
    false = is_map_key(N2, InMap),
    insert_link(NN#network{inputs=[N2|in_nodes(NN)]},start,N2);
add_link({N1, 'end'}, NN) ->
    OutMap = ?IN(map_get('end', NN#network.nodes)),
    false  = is_map_key(N1, OutMap),
    insert_link(NN#network{outputs=[N1|out_nodes(NN)]},N1,'end');
add_link({N1, N2}, NN) ->
    insert_link(NN, N1, N2).

%%-------------------------------------------------------------------
%% @doc Deletes the link between N1 and N2. 
%% @end
%%-------------------------------------------------------------------
-spec del_link(Link, NN1) -> NN2 when
    Link :: link:link(),
    NN1 :: network(),
    NN2 :: network().
del_link({start,   N2 }, NN) -> 
    remove_link(NN#network{ inputs=lists:delete(N2, in_nodes(NN))},start,N2);
del_link({   N1, 'end'}, NN) -> 
    remove_link(NN#network{outputs=lists:delete(N1,out_nodes(NN))},N1,'end');
del_link({   N1,   N2 }, NN) -> 
    remove_link(NN, N1, N2).

%%-------------------------------------------------------------------
%% @doc Deletes the link between N1 and N2. 
%% @end
%%-------------------------------------------------------------------
-spec move_link(Link, NN1, NMap) -> NN2 when
      Link  :: link:link(),
      NN1   :: network(),
      NMap  :: #{d_node() => d_node()},
      NN2   :: network().
move_link({N1, N2}, NN, NMap) -> 
    N3 = maps:get(N1, NMap, N1),
    N4 = maps:get(N2, NMap, N2),
    add_link({N3,N4}, del_link({N1,N2}, NN)).

%%-------------------------------------------------------------------
%% @doc Searches for a path between N1 and N2. 
%% @end
%%-------------------------------------------------------------------
-spec path({From, To}, NN) -> Path when
      From :: d_node(),
      To   :: d_node(),
      NN   :: network(),
      Path :: [d_node()] | not_found.
path({ N, N},  _) -> [N];
path({N1,N2}, NN) ->
    Nexts = out_nodes(N1, NN),
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
    [{N1,N2} || {N1,link} <- maps:to_list(In)].

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
    [{N1,N2} || {N2,link} <- maps:to_list(Out)].

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
        true  -> % That neuron were evluated before
            one_path(Ns, W, Cont, Xs, Ps, NN);
        false -> % That neuron out neighbours can be check firts
            Nexts = out_nodes(N,NN),
            one_path(Nexts, W, [{Ns,Ps}|Cont], [N|Xs], [N|Ps], NN)
    end;
one_path([], W, [{Ns,Ps}|Cont], Xs, _, NN) -> % End of neighbours
    one_path(Ns, W, Cont, Xs, Ps, NN);
one_path([], _,             [],  _, _,  _) -> % No seq path
    not_found.

% Inserts a link on the nodes ---------------------------------------
insert_link(#network{nodes=Nodes} = NN,  N,  N) ->
    #{N:= Conn} = Nodes,
    NN#network{nodes = Nodes#{
        N := add_out(add_in(Conn, N, link), N, link)
    }};
insert_link(#network{nodes=Nodes} = NN, N1, N2) ->
    #{N1:= ConnN1, N2:=ConnN2} = Nodes,
    NN#network{nodes = Nodes#{
        N1 := add_out(ConnN1, N2, link),
        N2 := add_in( ConnN2, N1, link)
    }}.

add_out(#cn{out=Out} = ConnN1, N2, What) -> 
    ConnN1#cn{out = Out#{N2 => What}}.

add_in(#cn{in=In} = ConnN2, N1, What) -> 
    ConnN2#cn{in = In#{N1 => What}}.

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

