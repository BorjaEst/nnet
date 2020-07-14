%%%-------------------------------------------------------------------
%%% @doc Set of functions to represent iun UML format a network.
%%% @end
%%%-------------------------------------------------------------------
-module(umlnn).

-export([print/1, print_rcc/1, print_seq/1]).


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Prints on shell the network and its connections in UML format.
%% @end
%%-------------------------------------------------------------------
-spec print(Id::nnet:id()) -> ok.
print(Id) -> 
    {atomic, _} = mnesia:transaction(
        fun() -> 
            Context = new_context(Id),
            format_seq(Context),
            format_rcc(Context)
        end),
    ok.

%%-------------------------------------------------------------------
%% @doc Prints on shell the network and its connections in UML format.
%% This case only prints the sequential connections.
%% @end
%%-------------------------------------------------------------------
-spec print_seq(Id::nnet:id()) -> ok.
print_seq(Id) -> 
    {atomic, _} = mnesia:transaction(
        fun() -> 
            Context = new_context(Id),
            format_seq(Context)
        end),
    ok. 

%%-------------------------------------------------------------------
%% @doc Prints on shell the network and its connections in UML format.
%% This case only prints the recurrent connections.
%% @end
%%-------------------------------------------------------------------
-spec print_rcc(Id::nnet:id()) -> ok.
print_rcc(Id) -> 
    {atomic, _} = mnesia:transaction(
        fun() -> 
            Context = new_context(Id),
            format_rcc(Context)
        end),
    ok. 


%%====================================================================
%% Context functions
%%====================================================================

% Creates a uml numeric id context from a network id ----------------
new_context(Id) -> 
    maps:fold(fun nid/3, #{id=>Id, next=>1}, nnet:nodes(Id)).

% Adds a ref into a uml context unique number ------------------------
nid(Ref,   nnode, #{next:=Nid} = Context) ->
    Context#{next:=Nid+1, Nid=>Ref, Ref=>Nid}.

% Maps a function over the network and its nodes --------------------
map(Fun, #{id:=Id} = Context) -> 
    NNodes = nnet:nodes(Id), 
    maps:map(fun(K,_) -> Fun(K,Context) end, NNodes#{Id=>network}).


%%====================================================================
%% Format functions
%%====================================================================

% Formats all the network sequential links --------------------------
format_seq(Context) -> map(fun format_seq/2, Context).

format_seq(NNode, Context) -> 
    [format_as_seq(L, Context) || L <-link:seq(NNode)].

% Formats a link into SEQ UML format using a context ----------------
format_as_seq({{network,_},B}, Context) -> 
    io:format("[start] --> ~w ~n", [map_get(B, Context)]);
format_as_seq({A,{network,_}}, Context) -> 
    io:format("~w --> [end] ~n",   [map_get(A, Context)]);
format_as_seq({A,B}, Context) -> 
    io:format("~w --> ~w ~n", [map_get(A, Context), map_get(B, Context)]).


% Formats all the network recurrent links ---------------------------
format_rcc(Context) -> map(fun format_rcc/2, Context).

format_rcc(NNode, Context) -> 
    [format_as_rcc(L, Context) || L <-link:rcc(NNode)].

% Formats a link into RCC UML format using a context ----------------
format_as_rcc({{network,_},B}, Context) -> 
    io:format("[start] ..> ~w ~n", [map_get(B, Context)]);
format_as_rcc({A,{network,_}}, Context) -> 
    io:format("~w ..> [end] ~n",   [map_get(A, Context)]);
format_as_rcc({A,B}, Context) -> 
    io:format("~w ..> ~w ~n", [map_get(A, Context), map_get(B, Context)]).


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

