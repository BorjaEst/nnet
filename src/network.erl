%%%-------------------------------------------------------------------
%%% @doc Module to manage network graph.
%%% @end
%%%-------------------------------------------------------------------
-module(network).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

-export([]).
-export_type([id/0, input/0, output/0]).

-type id()     :: {network, reference()}.
-type input()  :: nnode:id().
-type output() :: nnode:id().
-record(network, {
    key = make_ref() :: reference(),
    nnodes = #{} :: #{nnode:id() => nnode},
    inputs  = [] :: [nnode:id()], %Keeps the inputs order
    outputs = [] :: [nnode:id()]  %Keeps the outputs order
}).
-define(    KEY, element(    #network.key, Network)).
-define( NNODES, element( #network.nnodes, Network)).
-define( INPUTS, element( #network.inputs, Network)).
-define(OUTPUTS, element(#network.outputs, Network)).
-type info() :: #{'nnodes'  => #{nnode:id() => nnode},
                  'inputs'  => [nnode:id()],
                  'outputs' => [nnode:id()]}.


%%%===================================================================
%%% API: Dirty reads
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Mnesia table configuration for network.  
%% @end
%%-------------------------------------------------------------------
-spec table_configuration() -> MnesiaConfig::[term()].
table_configuration() -> 
    [
        {type, set},
        {attributes, record_info(fields, network)}
    ].

%%-------------------------------------------------------------------
%% @doc Information from the network.  
%% @end
%%-------------------------------------------------------------------
-spec info(Id::id()) -> Info::info().
info(Id) ->
    [Network] = mnesia:dirty_read(Id),
    #{
        nnodes  => ?NNODES,
        inputs  => ?INPUTS,
        outputs => ?OUTPUTS
    }.


%%%===================================================================
%%% API: Transactions
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Creates a new network.  
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec new() -> id().
new() -> 
    Network = #network{},
    ok = mnesia:write(Network),
    {network, ?KEY}.

%%-------------------------------------------------------------------
%% @doc Returns a copy of the network with a different id.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec clone(Id::id()) -> Copy::id().
clone(Id) -> 
    NN = read(Id),
    CopyKey = make_ref(),
    ok = write(NN#network{key=CopyKey}),
    {network, CopyKey}.

%%-------------------------------------------------------------------
%% @doc Renames the map nnodes in the network.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec rename(Id, NMap) -> Id when
    Id   :: id(),
    NMap :: #{Old::nnode:id() => New::nnode:id()}.
rename(Id, NMap) -> 
    Fun = fun(NN) -> priv_rename(NN, NMap) end,
    update_with(Id, Fun).

%%-------------------------------------------------------------------
%% @doc Deletes the network.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec delete(Id::id()) -> ok.
delete(Id) -> 
    ok = mnesia:delete(Id).

%%-------------------------------------------------------------------
%% @doc Adds a nnode to the network.  
%% @end
%%-------------------------------------------------------------------
-spec add_nnode(N::nnode:id(), Id::id()) -> Id::id().
add_nnode(N, Id) ->
    %% TODO: Raise error if already in?
    Fun = 
        fun(Network) -> 
            Network#network{nnodes = maps:put(N, node, ?NNODES)} 
        end,
    update_with(Id, Fun).

%%-------------------------------------------------------------------
%% @doc Deletes a nnode from the network.  
%% @end
%%-------------------------------------------------------------------
-spec del_nnode(N::nnode:id(), Id::id()) -> Id::id().
del_nnode(N, Id) ->
    Fun = 
        fun(Network) -> 
            Network#network{nnodes = maps:remove(N, ?NNODES)} 
        end,
    update_with(Id, Fun).

%%-------------------------------------------------------------------
%% @doc Returns the number of nnodes of the network.  
%% @end
%%-------------------------------------------------------------------
-spec size(Id::id()) -> Size::non_neg_integer().
size(Id) ->
    maps:size(nnodes(Id)).

%%-------------------------------------------------------------------
%% @doc Returns a list of all nnodes of the network.  
%% @end
%%-------------------------------------------------------------------
-spec nnodes(Id::id()) -> NNodes::#{nnode:id() => node}.
nnodes(Id) ->
    Network = read(Id),
    ?NNODES.

%%-------------------------------------------------------------------
%% @doc Adds a nnode to the network inputs. 
%% @end
%%-------------------------------------------------------------------
-spec add_input(N::nnode:id(), Id::id()) -> Id::id().
add_input(N, Id) ->
    Fun = 
        fun(Network) -> 
            Network#network{inputs = [N|?INPUTS]}
        end,
    update_with(Id, Fun). 

%%-------------------------------------------------------------------
%% @doc Deletes a nnode from the network inputs. 
%% @end
%%-------------------------------------------------------------------
-spec del_input(N::nnode:id(), Id::id()) -> Id::id().
del_input(N, Id) -> 
    Fun = 
        fun(Network) -> 
            Network#network{inputs = lists:delete(N, ?INPUTS)}
        end,
    update_with(Id, Fun).

%%-------------------------------------------------------------------
%% @doc Returns the nnodes connected to start. 
%% @end
%%-------------------------------------------------------------------
-spec inputs(Id::id()) -> NNodes::[nnode:id()].
inputs(Id) ->
    Network = read(Id),
    ?INPUTS.

%%-------------------------------------------------------------------
%% @doc Adds a nnode to the network outputs. 
%% @end
%%-------------------------------------------------------------------
-spec add_output(N::nnode:id(), Id::id()) -> Id::id().
add_output(N, Id) -> 
    Fun = 
        fun(Network) -> 
            Network#network{outputs = [N|?OUTPUTS]}
        end,
    update_with(Id, Fun). 

%%-------------------------------------------------------------------
%% @doc Deletes a nnode from the network output. 
%% @end
%%-------------------------------------------------------------------
-spec del_output(N::nnode:id(), Id::id()) -> Id::id().
del_output(N, Id) -> 
    Fun = 
        fun(Network) -> 
            Network#network{outputs = lists:delete(N, ?OUTPUTS)}
        end,
    update_with(Id, Fun).

%%-------------------------------------------------------------------
%% @doc Returns the nnodes connected to end. 
%% @end
%%-------------------------------------------------------------------
-spec outputs(Id::id()) -> NNodes::[nnode:id()].
outputs(Id) ->
    Network = read(Id),
    ?OUTPUTS.


%%====================================================================
%% Internal functions
%%====================================================================

% Reads the network from mnesia -------------------------------------
read({network, Key}) -> hd(mnesia:read({network, Key})).

% Wreads the network from mnesia ------------------------------------
wread({network, Key}) -> hd(mnesia:wread({network, Key})).

% Writes the network in mnesia --------------------------------------
write(#network{}=NN) -> ok = mnesia:write(NN).

% Update with a function --------------------------------------------
update_with({network, Key}, Fun) -> 
    NN = wread({network, Key}),
    ok = write(Fun(NN)),
    {network, Key}.

% Renames the network nodes -----------------------------------------
priv_rename(#network{} = NN, NMap) -> 
    NN#network{
        nnodes  = priv_rename(nnodes(NN), NMap),
        inputs  = priv_rename(inputs(NN), NMap),
        outputs = priv_rename(outputs(NN), NMap)
    };
priv_rename(#{} = NNodes, NMap) ->
    maps:from_list(
        [{maps:get(N,NMap,N),V} || {N,V} <- maps:to_list(NNodes)]
    );
priv_rename([_] = NList, NMap) -> 
    Rename = fun(N) -> maps:get(N, NMap, N) end,  
    lists:map(Rename, NList).


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

