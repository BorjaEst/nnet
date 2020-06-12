%%%-------------------------------------------------------------------
%%% @doc Module to manage network graph.
%%% @end
%%%-------------------------------------------------------------------
-module(network).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

-export([]).
-export_type([id/0, nnodes/0]).

-type id()     :: {network, reference()}.
-type nnodes() :: #{NNode::nnode:id() => nnode} .


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Mnesia table configuration for network.  
%% @end
%%-------------------------------------------------------------------
-spec table_configuration() -> MnesiaConfig::[term()].
table_configuration() -> 
    [
        {type, set}
    ].

%%-------------------------------------------------------------------
%% @doc Creates a new network.  
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec new() -> id().
new() -> 
    Key = make_ref(),
    ok  = mnesia:write({network, Key, #{}}),
    {network, Key}.

%%-------------------------------------------------------------------
%% @doc Returns a map of all nnodes of the network.  
%% @end
%%-------------------------------------------------------------------
-spec nnodes(Id::id()) -> NNodes::#{nnode:id() => nnode}.
nnodes({network, Key}) ->
    case mnesia:read({network, Key}) of 
        [{network, Key, NNodes}] -> NNodes;
        []                       -> error(not_found)
    end.

%%-------------------------------------------------------------------
%% @doc Returns a copy of the network with a different id.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec clone(Id::id()) -> Copy::id().
clone(Id) -> 
    Key = make_ref(),
    ok = mnesia:write({network, Key, nnodes(Id)}),
    {network, Key}.

%%-------------------------------------------------------------------
%% @doc Applies a function on the network nnodes map.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec update_with(Fun::function(), Id::id()) -> Id::id().
update_with(Fun, {network, Key}) -> 
    case mnesia:wread({network, Key}) of 
        [{network, Key, NNodes}] -> 
            ok = mnesia:write({network, Key, Fun(NNodes)}),
            {network, Key};
        [] -> error(not_found)
    end. 

%%-------------------------------------------------------------------
%% @doc Applies a function over all network nnodes.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec map(Fun::function(), Id::id()) -> Id::id().
map(Fun, {network, Key}) -> 
    case mnesia:wread({network, Key}) of 
        [{network, Key, NNodes}] -> 
            ok = mnesia:write({network, Key, maps:map(Fun, NNodes)}),
            {network, Key};
        [] -> error(not_found)
    end. 

%%-------------------------------------------------------------------
%% @doc Renames the map nnodes in the network.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec rename(Id, NMap) -> Id when
    Id   :: id(),
    NMap :: #{Old::nnode:id() => New::nnode:id()}.
rename(Id, NMap) -> 
    Rename = fun(N) -> maps:get(N, NMap, N) end, 
    map(Rename, Id).

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
    Add_NNode = fun(Ns) -> maps:put(N, nnode, Ns) end,
    update_with(Add_NNode, Id).

%%-------------------------------------------------------------------
%% @doc Deletes a nnode from the network.  
%% @end
%%-------------------------------------------------------------------
-spec del_nnode(N::nnode:id(), Id::id()) -> Id::id().
del_nnode(N, Id) ->
    Del_NNode = fun(Ns) -> maps:remove(N, Ns) end,
    update_with(Del_NNode, Id).

%%-------------------------------------------------------------------
%% @doc Returns the number of nnodes of the network.  
%% @end
%%-------------------------------------------------------------------
-spec size(Id::id()) -> Size::non_neg_integer().
size(Id) -> maps:size(nnodes(Id)).


%%====================================================================
%% Internal functions
%%====================================================================


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

