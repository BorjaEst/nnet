%%%-------------------------------------------------------------------
%%% @doc The neuron is a signal processing element. It accepts
%%% signals, accumulates them into an ordered vector, then processes
%%% this input vector to produce an output, and finally passes the
%%% output to other elements it is connected to.
%%% @end
%%%-------------------------------------------------------------------
-module(neuron).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

%% API
-export([]).
-export_type([id/0, properties/0]).

-type id() ::   {neuron, reference()}.
-type weight() :: number() | undefined.
-type properties() :: #{activation  := atom(),
                        aggregation := atom(),
                        initializer := atom(),
                        bias        := weight()}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new neuron. Some properties can be defined.
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
    ok = mnesia:write({neuron, Key, Data}),
    {neuron, Key}.

%%--------------------------------------------------------------------
%% @doc Returns the neuron properties.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec read(id()) -> properties().
read({neuron, Key}) -> 
    case mnesia:read({neuron, Key}) of 
        [{neuron, Key, Data}] -> Data;
        []                    -> error(not_found)
    end.

%%--------------------------------------------------------------------
%% @doc Returns a copy of the neuron with a different id.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec copy(Neuron::id()) -> Copy::id().
copy(Id) -> new(read(Id)).

%%--------------------------------------------------------------------
%% @doc Edits the neuron properties.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec edit(Id::id(), properties()) -> Id::id().
edit({neuron, Key}, Prop) -> 
    case mnesia:wread({neuron, Key}) of 
        [{neuron, Key, Data}] -> 
            ok = mnesia:write({neuron, Key, maps:merge(Data, Prop)}),
            {neuron, Key};
        []                    -> 
            error(not_found)
    end. 

%%--------------------------------------------------------------------
%% @doc Deletes the neuron and the properties.
%% Should run inside a mnesia transaction
%% @end
%%--------------------------------------------------------------------
-spec delete(Id::id()) -> ok.
delete({neuron, Key}) -> 
    ok = mnesia:delete({neuron, Key}),
    {neuron, Key}.


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

