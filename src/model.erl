%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(model).

%% API
-export([compile/1]).
-export_type([connections/0, density/0, nature/0, layer/0, model/0]).

-type nature()      :: sequential | recurrent | auto.
-type density()     :: float().
-type connections() :: #{Name::atom() => {nature(), density()} 
                                        | nature()}.
-type layer()       :: #{connections  := connections(),
                         units        := non_neg_integer(),
                         data         := NodeData::term()}.
-type model()       :: #{inputs       := Specs::layer(),
                         Name::atom() => Specs::layer(),
                         outputs      := Specs::layer()}. 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Compiles the network components and returns the network.
%% Should run inside a mnesia transaction.
%% @end
%%--------------------------------------------------------------------
-spec compile(Model::model()) -> Network::enn:network().
compile(#{inputs:=_, outputs:=_} = Model) -> 
    Layers = compile_layers(Model),
    NNet   = compile_network(Layers),
    ok = connect_nnodes(Layers),
    ok = connect_inputs( Layers, NNet) ,
    ok = connect_outputs(Layers, NNet),
    NNet.

% Adds the nodes to the nnode specs ---------------------------------
compile_layers(Model) -> 
    maps:map(fun compile/2, Model).
compile(_Layer, #{units:=N, data:=Data} = Definition) -> 
    Definition#{nnodes => [nnode:new(Data) || _ <- lists:seq(1, N)]}.

% Implements the neural network frm model ---------------------------
compile_network(Layers) -> 
    lists:foldl(fun add_nnodes/2, network:new(), maps:values(Layers)).

% Implements the layers connections ---------------------------------
connect_nnodes(Layers) ->
    Fun = fun(L) -> connect_nnodes(L, Layers) end,
    ok = lists:foreach(Fun, maps:keys(Layers)).

% Implements the network inputs connections -------------------------
connect_inputs(Layers, NNet) ->
    ok = nnet:connect_seq([{NNet,N} || N <- nnodes( inputs, Layers)]).

% Implements the network outputs connections ------------------------
connect_outputs(Layers, NNet) ->
    ok = nnet:connect_seq([{N,NNet} || N <- nnodes(outputs, Layers)]).


%%====================================================================
%% Internal functions
%%====================================================================

% Adds the nnodes to the nnode map key ------------------------------
add_nnodes(#{nnodes:=NNodes}, NNet) -> 
    lists:foldl(fun network:add_nnode/2, NNet, NNodes).

% Returns the nnodes from the Model map -----------------------------
nnodes(Layer, Layers) -> 
    #{Layer:=#{nnodes:=NNodes}} = Layers,
    NNodes.

% Implements a layer's connections ----------------------------------
connect_nnodes(L1, Layers) -> 
    #{L1:=#{nnodes:=Nx1, connections:=Connections}} = Layers,
    Connect = fun(L2,T) -> connect(Nx1,nnodes(L2,Layers),T) end,
    maps:map(Connect, Connections).

% Connects 2 Groups of neurons according to the nature and density --
connect(Nx1, Nx2, {sequential, Density}) -> 
    nnet:connect_seq(links(Nx1, Nx2, Density));
connect(Nx1, Nx2, {recurrent, Density}) ->  
    nnet:connect_rcc(links(Nx1, Nx2, Density));
connect(Nx1, Nx2, {auto, Density}) ->  
    nnet:connect(links(Nx1, Nx2, Density));
connect(Nx1, Nx2, Nature) -> 
    connect(Nx1, Nx2, {Nature, 1.0}).

% Retruns the list of links between 2 groups ------------------------
links(Nx1, Nx2, 1.0) -> 
    [{N1,N2} || N1 <- Nx1, N2 <- Nx2];
links(Nx1, Nx2, Probability) -> 
    [{N1,N2} || N1 <- Nx1, N2 <- Nx2, Probability > rand:uniform()].


%%====================================================================
%% Eunit white box tests
%%====================================================================

% -------------------------------------------------------------------
% TESTS DESCRIPTIONS ------------------------------------------------

% -------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ------------------------------------------

% -------------------------------------------------------------------
% ACTUAL TESTS ------------------------------------------------------

% -------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS -----------------------------------------

