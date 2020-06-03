%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc 
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(link).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

-export([]).
-export_type([from/0, to/0, weight/0]).

-type from()   :: term().
-type to()     :: term().
-type weight() :: number() | undefined.


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Returns the links weights.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec read({From, To}) -> weight() when 
    From :: nnode:id(),
    To   :: nnode:id().
read({From, To}) -> 
    case mnesia:read({link, {From,To}}) of 
        [{_,_,Weight}] -> Weight;
        []             -> undefined
    end.

%%-------------------------------------------------------------------
%% @doc Writes the indicated links in mnesia.
%% Should run inside a mnesia transaction.
%% @end
%%------------------------------------------------------------------
-spec write(Link, Weight) -> Link when
    Link   :: {from(), to()},
    Weight :: weight().
write({From, To}, Weight) -> 
    ok = mnesia:write({link,{From,To}, Weight}),
    {From, To}.

%%-------------------------------------------------------------------
%% @doc Adds a specific value to the previous weight.
%% Should run inside a mnesia transaction.
%% @end
%%------------------------------------------------------------------
-spec add(Link, Value) -> Link when
    Link  :: {from(), to()},
    Value :: weight().
add({From, To}, V) -> 
    case mnesia:wread({link, {From, To}}) of 
        [{_,_,Weight}] -> write({From, To}, Weight + V);
        []             -> write({From, To}, V)
    end.

%%-------------------------------------------------------------------
%% @doc Deletes the links (the status will be undefined).
%% Should run inside a mnesia transaction. 
%% @end
%%------------------------------------------------------------------
-spec delete(Link) -> ok when
    Link   :: {from(), to()}.
delete({From, To}) ->
    ok = mnesia:delete({link, {From, To}}),
    {From, To}.

%%-------------------------------------------------------------------
%% @doc Maps a link from a tuple {From,To}.
%% @end
%%------------------------------------------------------------------
-spec map(Link1, #{Old => New}) -> Link2 when 
    Link1 :: {Old::from(), Old::to()},
    Link2 :: {New::from(), New::to()},
    Old   :: from() | to(),
    New   :: from() | to().
map({From, To}, Map) ->
    {maps:get(From, Map, From), maps:get(To, Map, To)}.

%%-------------------------------------------------------------------
%% @doc Copies a link replacing the From and To ids using a map. 
%% Returns the maped link.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec copy(Link1, #{Old => New}) -> Link2 when 
    Link1 :: {Old::from(), Old::to()},
    Link2 :: {New::from(), New::to()},
    Old   :: from() | to(),
    New   :: from() | to().
copy({From, To}, Map) -> 
    case read({From, To}) of 
        W when is_float(W) -> write(map({From, To}, Map), W);
        undefined          -> map({From, To}, Map)
    end.

%%-------------------------------------------------------------------
%% @doc Moves the links using a map. Returns the maped link.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec move(Link1, #{Old => New}) -> Link2 when 
    Link1 :: {Old::from(), Old::to()},
    Link2 :: {New::from(), New::to()},
    Old   :: from() | to(),
    New   :: from() | to().
move({From, To}, Map) -> 
    NewLink = copy({From, To}, Map),
    delete({From, To}),
    NewLink.

%%-------------------------------------------------------------------
%% @doc Merges the links using a map. Returns the maped link.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec merge(Link1, #{Old => New}) -> Link2 when 
    Link1 :: {Old::from(), Old::to()},
    Link2 :: {New::from(), New::to()},
    Old   :: from() | to(),
    New   :: from() | to().
merge({From, To}, Map) -> 
    case read({From, To}) of 
        W when is_float(W) -> 
            delete({From, To}),
            add(map({From, To}, Map), W);
        undefined          -> 
            map({From, To}, Map)
    end.

%%-------------------------------------------------------------------
%% @doc Moves the 50% of the weights on the last mapped connections. 
%% Returns the maped link.
%% Should run inside a mnesia transaction.
%% @end
%%-------------------------------------------------------------------
-spec divide(Link1, #{Old => New}) -> Link2 when 
    Link1 :: {Old::from(), Old::to()},
    Link2 :: {New::from(), New::to()},
    Old   :: from() | to(),
    New   :: from() | to().
divide({From, To}, Map) -> 
    MapedLink = map({From, To}, Map),
    case mnesia:wread({link, {From, To}}) of 
        [{_,_,W}] when is_map_key(To, Map) -> 
            add(MapedLink, W); 
        [{_,_,W}] -> 
            write({From, To}, W/2.0),
            add(MapedLink, W/2.0);
        [] -> 
            nothing
    end,
    MapedLink.

%%====================================================================
%% Internal functions
%%====================================================================

