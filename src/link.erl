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
         []       -> error({not_defined, {link, Link}})
    end.

%%-------------------------------------------------------------------
%% @doc Writes the indicated link weight in mnesia.
%% Should run inside a mnesia transaction.
%% @end
%%------------------------------------------------------------------
-spec write(Link, Weight) -> ok | {error, Reason} when 
    Link   :: {from(), to()},
    Weight :: weight(),
    Reason :: {not_defined, {link, Link}}.
write(Link, Weight) -> 
    Write = fun(_) -> Weight end, 
    update_with(Write, Link).

%%-------------------------------------------------------------------
%% @doc Updates a specific weight using a function.
%% Should run inside a mnesia transaction.
%% @end
%%------------------------------------------------------------------
-spec update_with(Fun, Link) -> ok | {error, Reason} when 
    Link :: {from(), to()},
    Fun  :: function(),
    Reason :: {not_defined, {link, Link}}.
update_with(Fun, Link) -> 
    case mnesia:wread({weight, Link}) of 
        [{_,_,W}] -> ok = mnesia:write({weight, Link, Fun(W)});
         []       -> {error, {not_defined, {link, Link}}}
    end.

%%-------------------------------------------------------------------
%% @doc Returns the out links.
%% Should run inside a mnesia transaction.
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
%% Should run inside a mnesia transaction.
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
-spec copy(Link, #{Old => New}) -> ok | {error, Reason} when 
    Link :: {Old::from(), Old::to()},
    Old  :: from() | to(),
    New  :: from() | to(),
    Reason :: {not_defined, {link, Link}}.
copy(Link, NMap) -> 
    do_copy(Link, map(Link, NMap)).

do_copy( Link,  Link) -> ok;
do_copy(FLink, TLink) -> 
    case type(FLink) of 
        seq -> ok = add(TLink, seq, not_init);
        rcc -> ok = add(TLink, rcc, not_init);
        undefined -> {error, {not_defined, {link, FLink}}}
    end.

%%-------------------------------------------------------------------
%% @doc Copies a link and weight replacing the From and To as copy. 
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec clone(Link, #{Old => New}) -> ok | {error, Reason} when 
    Link :: {Old::from(), Old::to()},
    Old  :: from() | to(),
    New  :: from() | to(),
    Reason :: {not_defined, {link, Link}}.
clone(Link, NMap) ->
    do_clone(Link, map(Link, NMap)).

do_clone( Link,  Link) -> ok;
do_clone(FLink, TLink) -> 
    case type(FLink) of 
        seq -> ok = add(TLink, seq, read(FLink));
        rcc -> ok = add(TLink, rcc, read(FLink));
        undefined -> {error, {not_defined, {link, FLink}}}
    end.

%%-------------------------------------------------------------------
%% @doc Moves the links using a map.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec move(Link, #{Old => New}) -> ok | {error, Reason} when 
    Link :: {Old::from(), Old::to()},
    Old  :: from() | to(),
    New  :: from() | to(),
    Reason :: {not_defined, {link, Link}}.
move(Link, NMap) ->  
    do_move(Link, map(Link, NMap)).

do_move( Link,  Link) -> ok;
do_move(FLink, TLink) -> 
    case type(FLink) of 
        Type when Type==seq; Type==rcc -> 
            ok = merge(TLink, Type, read(FLink)), 
            ok = del(FLink);
        undefined -> 
            {error, {not_defined, {link, FLink}}}
    end.

%%-------------------------------------------------------------------
%% @doc Moves the 50% of the weights on the mapped connections. 
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec divide(Link, #{Old => New}) -> ok | {error, Reason} when 
    Link :: {Old::from(), Old::to()},
    Old  :: from() | to(),
    New  :: from() | to(),
    Reason :: {not_defined, {link, Link}}.
divide(Link, NMap) ->  
    do_divide(Link, map(Link, NMap)).

do_divide( Link,  Link) -> ok;
do_divide(FLink, TLink) -> 
    case type(FLink) of 
        Type when Type==seq; Type==rcc -> 
            {Xi, Ri} = takePer(read(FLink), 0.50),
            ok = merge(TLink, Type,  Xi), 
            ok = mnesia:write({weight, FLink, Ri});
        undefined -> 
            {error, {not_defined, {link, FLink}}}
    end.


%%====================================================================
%% Internal functions
%%====================================================================

% Maps a link from a tuple {From,To} --------------------------------
map({From, To}, Map) ->
    {maps:get(From, Map, From), maps:get(To, Map, To)}.

% Returns if {Tab,A,B} is in links table ----------------------------
is_in(Tab, A, B) -> 
    lists:any(fun({_,_,X}) -> X=:=B end, mnesia:read({Tab, A})).

% Returns the nature value from the list of out links ---------------
type({From, To}) -> 
    case is_in(link_seq, From, To) of 
        true  -> seq;
        false -> 
    case is_in(link_rcc, From, To) of 
        true  -> rcc;
        false -> undefined
    end end.

% Merges the Link with W1 -------------------------------------------
merge({From, To} = Link, Type, W1) -> 
    case Type of 
        seq -> ok = mnesia:write({link_seq, From, To});
        rcc -> ok = mnesia:write({link_rcc, From, To})
    end,
    case mnesia:wread({weight, Link}) of 
        [{_,_,W2}] -> ok = mnesia:write({weight, Link, merge(W1,W2)});
        []         -> ok = mnesia:write({weight, Link, W1}),
                      ok = mnesia:write({link_in, To, From})
    end.

merge(not_init, W2) -> W2;
merge(W1, not_init) -> W1;
merge(W1, W2)       -> W1+W2.

% Takes th percentage of a weight ------------------
takePer(not_init, _) ->  {not_init, not_init};
takePer(      W1, X) -> Xi = X*W1, {Xi,W1-Xi}.


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

