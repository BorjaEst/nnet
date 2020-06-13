%%%-------------------------------------------------------------------
%%% @doc The nnode is a signal processing element. It accepts
%%% signals, accumulates them into an ordered vector, then processes
%%% this input vector to produce an output, and finally passes the
%%% output to other elements it is connected to.
%%% @end
%%%-------------------------------------------------------------------
-module(nnode).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

%% API
-export([]).
-export_type([id/0, data/0]).

-type id()   :: {nnode, reference()}.
-type data() :: #{}.


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

%%--------------------------------------------------------------------
%% @doc Creates a new nnode. Some properties can be defined.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec new(Data::data()) -> Id::id().
new(#{} = Data) ->
    Key = make_ref(),
    ok  = mnesia:write({nnode, Key, Data}),
    {nnode, Key}.

%%--------------------------------------------------------------------
%% @doc Returns the nnode properties.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec read(Id::id()) -> Data::data().
read({nnode, Key}) -> 
    case mnesia:read({nnode, Key}) of 
        [{nnode, Key, Data}] -> Data;
        []                    -> error(not_found)
    end.

%%--------------------------------------------------------------------
%% @doc Returns a copy of the nnode with a different id.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec clone(NNode::id()) -> Copy::id().
clone(Id) -> new(read(Id)).

%%--------------------------------------------------------------------
%% @doc Edits the nnode properties.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec edit(Id::id(), NewData::data()) -> ok.
edit({nnode, Key}, Prop) -> 
    case mnesia:wread({nnode, Key}) of 
        [{nnode, Key, Data}] -> 
            mnesia:write({nnode, Key, maps:merge(Data, Prop)});
        []                    -> 
            error(not_found)
    end. 

%%--------------------------------------------------------------------
%% @doc Deletes the nnode and the properties.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec delete(Id::id()) -> ok.
delete({nnode, Key}) -> 
    ok = mnesia:delete({nnode, Key}).


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

