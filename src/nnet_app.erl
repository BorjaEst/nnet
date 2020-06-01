%%%-------------------------------------------------------------------
%% @doc nnet public API
%% TODO: Optimise accross nodes: Eval usage of {local_content, true}  
%% @end
%%%-------------------------------------------------------------------
-module(nnet_app).
-author("borja").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    true = new_table(network, network:record_fields()),
    true = new_table(   link,     [  from_to, weight]),
    true = new_table( neuron,     [reference,   data]),
    nnet_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

% Creates a new table -----------------------------------------------
new_table(Name, Attributes) ->
    case mnesia:create_table(Name, [{attributes, Attributes}]) of
        {atomic, ok} -> true;
        {aborted, {already_exists, Name}} -> check(Name, Attributes);
        Other -> Other
    end.

% Checks the table has the correct attributes -----------------------
-define(BAD_TABLE, "table ~s exists using invalid attributtes").
check(Name, Attributes) ->
    case mnesia:table_info(Name, attributes) of 
        Attributes -> true;
        _ -> exit(io_lib:format(?BAD_TABLE, [Name]))
    end.

