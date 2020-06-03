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
-export_type([id/0, properties/0]).

-type id() ::   {nnode, reference()}.
-type weight() :: number() | undefined.
-type properties() :: #{activation  := atom(),
                        aggregation := atom(),
                        initializer := atom(),
                        bias        := weight()}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new nnode. Some properties can be defined.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec new(properties()) -> id().
new(Properties) ->
    Key  = make_ref(),
    Data = #{
        activation  => maps:get( activation, Properties,    direct),
        aggregation => maps:get(aggregation, Properties,  dot_prod),
        initializer => maps:get(initializer, Properties,    glorot),
        bias        => maps:get(initializer, Properties, undefined)
    },
    ok = mnesia:write({nnode, Key, Data}),
    {nnode, Key}.

%%--------------------------------------------------------------------
%% @doc Returns the nnode properties.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec read(id()) -> properties().
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
-spec copy(Nnode::id()) -> Copy::id().
copy(Id) -> new(read(Id)).

%%--------------------------------------------------------------------
%% @doc Edits the nnode properties.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec edit(Id::id(), properties()) -> Id::id().
edit({nnode, Key}, Prop) -> 
    case mnesia:wread({nnode, Key}) of 
        [{nnode, Key, Data}] -> 
            ok = mnesia:write({nnode, Key, maps:merge(Data, Prop)}),
            {nnode, Key};
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
    ok = mnesia:delete({nnode, Key}),
    {nnode, Key}.


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

