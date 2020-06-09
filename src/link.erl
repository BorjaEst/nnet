%%%-------------------------------------------------------------------
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(link).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

-export([]).
-export_type([from/0, to/0, weight/0, nature/0]).

-type from()   :: term().
-type to()     :: term().
-type weight() :: number() | not_init.
-type nature() :: seq | rcc.


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Mnesia tables configuration for links.  
%% @end
%%-------------------------------------------------------------------
-spec table_in_configuration() -> MnesiaConfig::[term()].
table_in_configuration() -> 
    [{type, bag},
     {attributes, [to, from]}].

-spec table_seq_configuration() -> MnesiaConfig::[term()].
table_seq_configuration() -> 
    [{type, bag},
     {attributes, [from, to]}].

-spec table_rcc_configuration() -> MnesiaConfig::[term()].
table_rcc_configuration() -> 
    [{type, bag},
     {attributes, [from, to]}].

-spec table_weight_configuration() -> MnesiaConfig::[term()].
table_weight_configuration() -> 
    [{type, set},
     {attributes, [link, weight]}].

%%-------------------------------------------------------------------
%% @doc Adds a specific value to the previous weight.
%% Should run inside a mnesia transaction.
%% @end
%%------------------------------------------------------------------
-spec add(Link, Type, Weight) -> ok when
    Link   :: {from(), to()},
    Type   :: nature(),
    Weight :: weight().
add({From, To} = Link, Type, Weight) -> 
    ok = mnesia:write({ weight, Link, Weight}),
    ok = mnesia:write({link_in,   To,   From}),
    case Type of 
        seq -> ok = mnesia:write({link_seq, From, To});
        rcc -> ok = mnesia:write({link_rcc, From, To})
    end.

%%-------------------------------------------------------------------
%% @doc Deletes the links (the status will be undefined).
%% Should run inside a mnesia transaction. 
%% @end
%%------------------------------------------------------------------
-spec del(Link) -> ok when
    Link   :: {from(), to()}.
del({From, To} = Link) ->
    ok = mnesia:delete({weight, Link}),
    ok = mnesia:delete_object({ link_in, To, From}),
    ok = mnesia:delete_object({link_seq, From, To}),
    ok = mnesia:delete_object({link_rcc, From, To}).

%%-------------------------------------------------------------------
%% @doc Returns the link weight.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec read(Link) -> Weight when 
    Link   :: {from(), to()},
    Weight :: weight().
read(Link) -> 
    case mnesia:read({weight, Link}) of 
        [{_,_,W}] -> W;
         []       -> error({{link, Link}, not_defined})
    end.

%%-------------------------------------------------------------------
%% @doc Returns the link weight.
%% @end
%%-------------------------------------------------------------------
-spec dirty_read(Link) -> Weight when 
    Link   :: {from(), to()},
    Weight :: weight().
dirty_read(Link) ->  
    case mnesia:dirty_read({weight, Link}) of 
        [{_,_,W}] -> W;
         []       -> error({{link, Link}, not_defined})
    end.

%%-------------------------------------------------------------------
%% @doc Writes the indicated link weight in mnesia.
%% Should run inside a mnesia transaction.
%% @end
%%------------------------------------------------------------------
-spec write(Link, Weight) -> ok when
    Link   :: {from(), to()},
    Weight :: weight().
write(Link, Weight) -> 
    ok = mnesia:write({weight, Link, Weight}).

%%-------------------------------------------------------------------
%% @doc Writes the indicated link weight in mnesia.
%% Should run inside a mnesia transaction.
%% @end
%%------------------------------------------------------------------
-spec dirty_write(Link, Weight) -> ok when
    Link   :: {from(), to()},
    Weight :: weight().
dirty_write(Link, Weight) -> 
    ok = mnesia:dirty_write({weight, Link, Weight}).

%%-------------------------------------------------------------------
%% @doc Updates a specific weight using a function.
%% Should run inside a mnesia transaction.
%% @end
%%------------------------------------------------------------------
-spec update_with(Link, Fun) -> ok when
    Link :: {from(), to()},
    Fun  :: weight().
update_with(Link, Fun) -> 
    case mnesia:wread({weight, Link}) of 
        [{_,_,W}] -> write(Link, Fun(W));
         []       -> error({{link, Link}, not_defined})
    end.

%%-------------------------------------------------------------------
%% @doc Returns the out links.
%% Should run inside a nnet edit.
%% @end
%%-------------------------------------------------------------------
-spec seq(From::from()) -> Out::[{From::from(), To::to()}].
seq(From) -> 
    [{From,To} || {_,_,To} <- mnesia:read({link_seq, From})].

-spec rcc(From::from()) -> Out::[to()].
rcc(From) -> 
    [{From,To} || {_,_,To} <- mnesia:read({link_rcc, From})].

%%-------------------------------------------------------------------
%% @doc Returns the in links.
%% Should run inside a nnet edit.
%% @end
%%-------------------------------------------------------------------
-spec in(To::to()) -> In::[{From::from(), To::to()}].
in(To) -> 
    [{From,To} || {_,_,From} <- mnesia:read({link_in, To})].

%%-------------------------------------------------------------------
%% @doc Copies a link replacing the From and To ids using a map. 
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec copy(Link, #{Old => New}) -> ok when 
    Link :: {Old::from(), Old::to()},
    Old  :: from() | to(),
    New  :: from() | to().
copy(Link, NMap) -> 
    ok = add(map(Link, NMap), type(Link), not_init).

%%-------------------------------------------------------------------
%% @doc Copies a link and weight replacing the From and To as copy. 
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec clone(Link, #{Old => New}) -> ok when 
    Link :: {Old::from(), Old::to()},
    Old  :: from() | to(),
    New  :: from() | to().
clone(Link, NMap) -> 
    ok = add(map(Link, NMap), type(Link), read(Link)).

%%-------------------------------------------------------------------
%% @doc Moves the links using a map.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec move(Link, #{Old => New}) -> ok when 
    Link :: {Old::from(), Old::to()},
    Old  :: from() | to(),
    New  :: from() | to().
move(Link, NMap) -> 
    ok = clone(Link, NMap),
    ok = del(Link).

%%-------------------------------------------------------------------
%% @doc Moves the 50% of the weights on the mapped connections. 
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec divide(Link, #{Old => New}) -> ok when 
    Link :: {Old::from(), Old::to()},
    Old  :: from() | to(),
    New  :: from() | to().
divide(Link, NMap) -> 
    ok = update_with(Link, fun per50/1),
    ok = clone(Link, NMap).

per50(Value) -> Value *0.50.

%%-------------------------------------------------------------------
%% @doc Merges the links using a map.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec merge(Link, #{Old => New}) -> ok when 
    Link :: {Old::from(), Old::to()},
    Old  :: from() | to(),
    New  :: from() | to().
merge(Link, NMap) ->
    update_with(map(Link, NMap), fun(X) -> lsum(X,read(Link)) end),
    del(Link).

lsum(      W1, not_init) -> W1;  
lsum(not_init,       W2) ->    W2;  
lsum(      W1,       W2) -> W1+W2.


%%====================================================================
%% Internal functions
%%====================================================================

% Returns the nature value from the list of out links ---------------
type({From, To} = Link) -> 
    case is_in(link_seq, From, To) of 
        true  -> seq;
        false -> 
    case is_in(link_rcc, From, To) of 
        true  -> rcc;
        false -> error({{link, Link}, not_defined})
    end end.

% Returns if {Tab,A,B} is in links table ----------------------------
is_in(Tab, A, B) -> 
    lists:any(fun({_,_,X}) -> X=:=B end, mnesia:read({Tab, A})).

% Maps a link from a tuple {From,To} --------------------------------
map({From, To}, Map) ->
    {maps:get(From, Map, From), maps:get(To, Map, To)}.


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

