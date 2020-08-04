%%%-------------------------------------------------------------------
%%% @doc Set of functions to represent iun UML format a network.
%%% @end
%%%-------------------------------------------------------------------
-module(umlnn).

-export([ print/1,  print_rcc/1,  print_seq/1]).
-export([format/1, format_rcc/1, format_seq/1]).


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Prints on shell the network and its connections in UML format.
%% @end
%%-------------------------------------------------------------------
-spec print(Id::nnet:id()) -> ok.
print(Id) -> 
    {atomic, Format} = mnesia:transaction(
        fun() -> format(Id) end
    ),
    ok = io:format(Format).

%%-------------------------------------------------------------------
%% @doc Prints on shell the network and its connections in UML format.
%% This case only prints the sequential connections.
%% @end
%%-------------------------------------------------------------------
-spec print_seq(Id::nnet:id()) -> ok.
print_seq(Id) -> 
    {atomic, Format} = mnesia:transaction(
        fun() -> format_seq(Id) end
    ),
    ok = io:format(Format). 

%%-------------------------------------------------------------------
%% @doc Prints on shell the network and its connections in UML format.
%% This case only prints the recurrent connections.
%% @end
%%-------------------------------------------------------------------
-spec print_rcc(Id::nnet:id()) -> ok.
print_rcc(Id) -> 
    {atomic, Format} = mnesia:transaction(
        fun() -> format_rcc(Id) end
    ),
    ok = io:format(Format). 

%%-------------------------------------------------------------------
%% @doc Returns the network print format in UML format.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec format(Id::nnet:id()) -> io_lib:chars().
format(Id) -> 
    Context = new_context(Id),
    [do_format_seq(Context),
     do_format_rcc(Context)].

%%-------------------------------------------------------------------
%% @doc Prints on shell the network and its connections in UML format.
%% This case only prints the sequential connections.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec format_seq(Id::nnet:id()) -> io_lib:chars().
format_seq(Id) -> 
    Context = new_context(Id),
    do_format_seq(Context).

%%-------------------------------------------------------------------
%% @doc Prints on shell the network and its connections in UML format.
%% This case only prints the recurrent connections.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec format_rcc(Id::nnet:id()) -> io_lib:chars().
format_rcc(Id) -> 
    Context = new_context(Id),
    do_format_rcc(Context).


%%====================================================================
%% Context functions
%%====================================================================

% Creates a uml numeric id context from a network id ----------------
new_context(Id) -> 
    maps:fold(fun nid/3, #{id=>Id, next=>1}, nnet:nodes(Id)).

% Adds a ref into a uml context unique number ------------------------
nid(Ref,   nnode, #{next:=Nid} = Context) ->
    Context#{next:=Nid+1, Nid=>Ref, Ref=>Nid}.


%%====================================================================
%% Format functions
%%====================================================================

% Formats all the network sequential links --------------------------
do_format_seq(#{id:=Id} = Context) ->
    [format_as_seq(L, Context) || 
        Node <- [Id|maps:keys(nnet:nodes(Id))], 
           L <- link:seq(Node)
    ].

% Formats a link into SEQ UML format using a context ----------------
format_as_seq({{network,_},B}, Context) -> 
    io_lib:format("[start] --> ~w ~n", [map_get(B, Context)]);
format_as_seq({A,{network,_}}, Context) -> 
    io_lib:format("~w --> [end] ~n",   [map_get(A, Context)]);
format_as_seq({A,B}, Context) -> 
    io_lib:format("~w --> ~w ~n", [map_get(A, Context), map_get(B, Context)]).


% Formats all the network recurrent links ---------------------------
do_format_rcc(#{id:=Id} = Context) -> 
    [format_as_rcc(L, Context) || 
        Node <- [Id|maps:keys(nnet:nodes(Id))],
           L <- link:rcc(Node)
    ].

% Formats a link into RCC UML format using a context ----------------
format_as_rcc({{network,_},B}, Context) -> 
    io_lib:format("[start] ..> ~w ~n", [map_get(B, Context)]);
format_as_rcc({A,{network,_}}, Context) -> 
    io_lib:format("~w ..> [end] ~n",   [map_get(A, Context)]);
format_as_rcc({A,B}, Context) -> 
    io_lib:format("~w ..> ~w ~n", [map_get(A, Context), map_get(B, Context)]).


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

