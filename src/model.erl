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
    NNet_1 = compile_network(Layers),
    NNet_2 = connect([start],nnodes(inputs, Layers),sequential,NNet_1),
    _Net_3 = connect(nnodes(outputs, Layers),['end'],sequential,NNet_2).

% Adds the nodes to the nnode specs ---------------------------------
compile_layers(Model) -> 
    maps:map(fun compile/2, Model).
compile(_Layer, #{units:=N, data:=Data} = Definition) -> 
    Definition#{nnodes => [nnode:new(Data) || _ <- lists:seq(1, N)]}.

% Implements the neural network frm model ---------------------------
compile_network(Model) -> 
    NNet = maps:fold(fun add_nnodes/3, network:new(), Model),
    connect_nodes(NNet, Model).


%%====================================================================
%% Internal functions
%%====================================================================

% Adds the nnodes to the nnode map key ------------------------------
add_nnodes(_Layer, #{nnodes:=NNodes}, NNet) -> 
    lists:foldl(fun network:add_nnode/2, NNet, NNodes).

% Returns the nnodes from the Model map -----------------------------
nnodes(Layer, Model) -> 
    #{Layer:=#{nnodes:=NNodes}} = Model,
    NNodes.

% Implements the layers connections ---------------------------------
connect_nodes(NNet, Model) ->
    Fun = fun(L,NN) -> connect_nnodes(L,NN,Model) end,
    lists:foldl(Fun, NNet, maps:keys(Model)).
connect_nnodes(L1, NNet, Model) -> 
    #{L1:=#{nnodes:=Nx1, connections:=Connections}} = Model,
    Connect = fun(L2,T,NN) -> connect(Nx1,nnodes(L2,Model),T,NN) end,
    maps:fold(Connect, NNet, Connections).

% Connects 2 Groups of neurons according to the nature and density --
connect(Nx1, Nx2, {sequential, Density}, NNet) -> 
    nnet:connect_seq(links(Nx1,Nx2,Density), NNet);
connect(Nx1, Nx2, {recurrent, Density}, NNet) ->  
    nnet:connect_rcc(links(Nx1,Nx2,Density), NNet);
connect(Nx1, Nx2, {auto, Density}, NNet) ->  
    nnet:connect(links(Nx1,Nx2,Density), NNet);
connect(Nx1, Nx2, Nature, NNet) -> 
    connect(Nx1, Nx2, {Nature, 1.0}, NNet).

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

