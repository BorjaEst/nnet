%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(network).

-export([new/0, new/1, delete/1, info/1]).

-export([add_neuron/1, add_neuron/2, add_neuron/3]).
-export([del_neuron/2, del_neurons/2]).
-export([neuron/2, no_neurons/1, neurons/1]).
-export([source_neurons/1, sink_neurons/1]).

-export([add_conn/3, add_conn/4, add_conn/5]).
-export([del_conn/2, del_conns/2, del_path/3]).
-export([conn/2, no_conns/1, conns/1]).

-export([out_neighbours/2, in_neighbours/2]).
-export([out_conns/2, in_conns/2, conns/2]).
-export([out_degree/2, in_degree/2]).
-export([get_path/3, get_cycle/2]).

-export([get_short_path/3, get_short_cycle/2]).

-export_type([network/0, d_type/0, neuron/0, conn/0, label/0]).

-record(network, {
    vtab = notable   :: ets:tab(),
    ctab = notable   :: ets:tab(),
    ntab = notable   :: ets:tab(),
    recurrent = true :: boolean()
}).

-opaque network() :: #network{}.

-type conn()    :: term().
-type label()   :: term().
-type neuron()  :: term().

-type d_type()  :: 'sequential' | 'recurrent'.

-type add_conn_err_rsn() :: {'bad_conn', Path :: [neuron()]}
                          | {'bad_neuron',  V ::  neuron() }.

-define(VTAB_CONFIGUTATION, [set, public, { read_concurrency,true}]).
-define(CTAB_CONFIGUTATION, [set, public, {write_concurrency,true}]).
-define(NTAB_CONFIGUTATION, [set, public, {write_concurrency,true}]).


%%%===================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Creates a new network.  
%% @end
%%-------------------------------------------------------------------
-spec new() -> network().
new() -> new(recurrent).

-spec new(Type) -> network() when
      Type :: [d_type()].
new(Type) ->
    case check_type(Type, []) of
    {ok, Ts} ->
        V = ets:new(   neurons, ?VTAB_CONFIGUTATION),
        C = ets:new(     conns, ?CTAB_CONFIGUTATION),
        NN = ets:new(neighbours, ?CTAB_CONFIGUTATION),
        ets:insert(NN, [{'$vid', 0}, {'$eid', 0}]),
        set_type(Ts, #network{vtab=V, ctab=C, ntab=NN});
    error ->
        erlang:error(badarg)
    end.

check_type([sequential| Ts], L) -> check_type(Ts, [{recurrent,false} | L]);
check_type([recurrent | Ts], L) -> check_type(Ts, [{recurrent, true} | L]);
check_type(              [], L) -> {ok, L};
check_type(               _, _) -> error.

set_type([{recurrent,V} | Ks], NN) -> set_type(Ks, NN#network{recurrent = V});
set_type(                  [], NN) -> NN.

%%-------------------------------------------------------------------
%% @doc Deletes a network.  
%% @end
%%-------------------------------------------------------------------
-spec delete(NN) -> 'true' when
      NN :: network().
delete(NN) ->
    ets:delete(NN#network.vtab),
    ets:delete(NN#network.ctab),
    ets:delete(NN#network.ntab).

%%-------------------------------------------------------------------
%% @doc Information from the network.  
%% @end
%%-------------------------------------------------------------------
-spec info(NN) -> InfoList when
      NN :: network(),
      InfoList :: [{'type', Type :: d_type()} |
                   {'memory', NoWords :: non_neg_integer()}].
info(NN) ->
    VT = NN#network.vtab,
    ET = NN#network.ctab,
    NT = NN#network.ntab,
    Type = case NN#network.recurrent of
            true  -> recurrent;
            false -> sequential
        end,
    Protection = ets:info(VT, protection),
    Memory = ets:info(VT,memory)+ets:info(ET,memory)+ets:info(NT,memory),
    [{type, Type}, {memory, Memory}, {protection, Protection}].

%%-------------------------------------------------------------------
%% @doc Adds a neuron to the network.  
%% @end
%%-------------------------------------------------------------------
-spec add_neuron(NN) -> neuron() when
      NN :: network().
add_neuron(NN) ->
    do_add_neuron({new_neuron_id(NN), []}, NN).

-spec add_neuron(NN, V) -> neuron() when
      NN :: network(),
      V :: neuron().
add_neuron(NN, V) ->
    do_add_neuron({V, []}, NN).

-spec add_neuron(NN, V, Label) -> neuron() when
      NN :: network(),
      V :: neuron(),
      Label :: label().
add_neuron(NN, V, D) ->
    do_add_neuron({V, D}, NN).


%%
%% Generate a "unique" neuron identifier (relative to this network)
%%
-dialyzer({no_improper_lists, new_neuron_id/1}).
-spec new_neuron_id(network()) -> neuron().
new_neuron_id(NN) ->
    NT = NN#network.ntab,
    [{'$vid', K}] = ets:lookup(NT, '$vid'),
    true = ets:delete(NT, '$vid'),
    true = ets:insert(NT, {'$vid', K+1}),
    ['$v' | K].


-spec do_add_neuron({neuron(), label()}, network()) -> neuron().
do_add_neuron({V, _Label} = VL, NN) ->
    ets:insert(NN#network.vtab, VL),
    V.











-spec del_neuron(NN, V) -> 'true' when
      NN :: network(),
      V :: neuron().
del_neuron(NN, V) ->
    do_del_neuron(V, NN).

-spec del_neurons(NN, Neurons) -> 'true' when
      NN :: network(),
      Neurons :: [neuron()].
del_neurons(NN, Vs) -> 
    do_del_neurons(Vs, NN).

-spec neuron(NN, V) -> {V, Label} | 'false' when
      NN :: network(),
      V :: neuron(),
      Label :: label().
neuron(NN, V) ->
    case ets:lookup(NN#network.vtab, V) of
    [] -> false;
    [Neuron] -> Neuron
    end.

-spec no_neurons(NN) -> non_neg_integer() when
      NN :: network().

no_neurons(NN) ->
    ets:info(NN#network.vtab, size).

-spec neurons(NN) -> Neurons when
      NN :: network(),
      Neurons :: [neuron()].

neurons(NN) ->
    ets:select(NN#network.vtab, [{{'$1', '_'}, [], ['$1']}]).

-spec source_neurons(network()) -> [neuron()].

source_neurons(NN) ->
    collect_neurons(NN, in).

-spec sink_neurons(network()) -> [neuron()].

sink_neurons(NN) ->
    collect_neurons(NN, out).

-spec in_degree(NN, V) -> non_neg_integer() when
      NN :: network(),
      V :: neuron().

in_degree(NN, V) ->
    length(ets:lookup(NN#network.ntab, {in, V})).

-spec in_neighbours(NN, V) -> Neuron when
      NN :: network(),
      V :: neuron(),
      Neuron :: [neuron()].

in_neighbours(NN, V) ->
    ET = NN#network.ctab,
    NT = NN#network.ntab,
    collect_elems(ets:lookup(NT, {in, V}), ET, 2).

-spec in_conns(NN, V) -> Conns when
      NN :: network(),
      V :: neuron(),
      Conns :: [conn()].

in_conns(NN, V) ->
    ets:select(NN#network.ntab, [{{{in, V}, '$1'}, [], ['$1']}]).

-spec out_degree(NN, V) -> non_neg_integer() when
      NN :: network(),
      V :: neuron().

out_degree(NN, V) ->
    length(ets:lookup(NN#network.ntab, {out, V})).

-spec out_neighbours(NN, V) -> Neurons when
      NN :: network(),
      V :: neuron(),
      Neurons :: [neuron()].

out_neighbours(NN, V) ->
    ET = NN#network.ctab,
    NT = NN#network.ntab,
    collect_elems(ets:lookup(NT, {out, V}), ET, 3).

-spec out_conns(NN, V) -> Conns when
      NN :: network(),
      V :: neuron(),
      Conns :: [conn()].

out_conns(NN, V) ->
    ets:select(NN#network.ntab, [{{{out, V}, '$1'}, [], ['$1']}]).

-spec add_conn(NN, V1, V2) -> conn() | {'error', add_conn_err_rsn()} when
      NN :: network(),
      V1 :: neuron(),
      V2 :: neuron().

add_conn(NN, V1, V2) ->
    do_add_conn({new_conn_id(NN), V1, V2, []}, NN).

-spec add_conn(NN, V1, V2, Label) -> conn() | {'error', add_conn_err_rsn()} when
      NN :: network(),
      V1 :: neuron(),
      V2 :: neuron(),
      Label :: label().

add_conn(NN, V1, V2, D) ->
    do_add_conn({new_conn_id(NN), V1, V2, D}, NN).

-spec add_conn(NN, C, V1, V2, Label) -> conn() | {'error', add_conn_err_rsn()} when
      NN :: network(),
      C :: conn(),
      V1 :: neuron(),
      V2 :: neuron(),
      Label :: label().

add_conn(NN, C, V1, V2, D) ->
    do_add_conn({C, V1, V2, D}, NN).

-spec del_conn(NN, C) -> 'true' when
      NN :: network(),
      C :: conn().

del_conn(NN, C) ->
    do_del_conns([C], NN).

-spec del_conns(NN, Conns) -> 'true' when
      NN :: network(),
      Conns :: [conn()].

del_conns(NN, Es) ->
    do_del_conns(Es, NN).

-spec no_conns(NN) -> non_neg_integer() when
      NN :: network().

no_conns(NN) ->
    ets:info(NN#network.ctab, size).

-spec conns(NN) -> Conns when
      NN :: network(),
      Conns :: [conn()].

conns(NN) ->
    ets:select(NN#network.ctab, [{{'$1', '_', '_', '_'}, [], ['$1']}]).

-spec conns(NN, V) -> Conns when
      NN :: network(),
      V :: neuron(),
      Conns :: [conn()].

conns(NN, V) ->
    ets:select(NN#network.ntab, [{{{out, V},'$1'}, [], ['$1']},
                {{{in, V}, '$1'}, [], ['$1']}]).

-spec conn(NN, C) -> {C, V1, V2, Label} | 'false' when
      NN :: network(),
      C :: conn(),
      V1 :: neuron(),
      V2 :: neuron(),
      Label :: label().

conn(NN, C) ->
    case ets:lookup(NN#network.ctab,C) of
    [] -> false;
    [Conn] -> Conn
    end.

%%
%% Generate a "unique" conn identifier (relative to this network)
%%
-spec new_conn_id(network()) -> conn().

-dialyzer({no_improper_lists, new_conn_id/1}).

new_conn_id(NN) ->
    NT = NN#network.ntab,
    [{'$eid', K}] = ets:lookup(NT, '$eid'),
    true = ets:delete(NT, '$eid'),
    true = ets:insert(NT, {'$eid', K+1}),
    ['$e' | K].

%%
%% Collect elements for a index in a tuple
%%
collect_elems(Keys, Table, Index) ->
    collect_elems(Keys, Table, Index, []).

collect_elems([{_,Key}|Keys], Table, Index, Acc) ->
    collect_elems(Keys, Table, Index,
          [ets:lookup_element(Table, Key, Index)|Acc]);
collect_elems([], _, _, Acc) -> Acc.



%%
%% Collect either source or sink neurons.
%%
collect_neurons(NN, Type) ->
    Vs = neurons(NN),
    lists:foldl(fun(V, A) ->
            case ets:member(NN#network.ntab, {Type, V}) of
                true -> A;
                false -> [V|A]
            end
        end, [], Vs).

%%
%% Delete neurons
%%
do_del_neurons([V | Vs], NN) ->
    do_del_neuron(V, NN),
    do_del_neurons(Vs, NN);
do_del_neurons([], #network{}) -> true.

do_del_neuron(V, NN) ->
    do_del_nconns(ets:lookup(NN#network.ntab, {in, V}), NN),
    do_del_nconns(ets:lookup(NN#network.ntab, {out, V}), NN),
    ets:delete(NN#network.vtab, V).

do_del_nconns([{_, C}|Ns], NN) ->
    case ets:lookup(NN#network.ctab, C) of
    [{C, V1, V2, _}] ->
        do_del_conn(C, V1, V2, NN),
        do_del_nconns(Ns, NN);
    [] -> % cannot happen
        do_del_nconns(Ns, NN)
    end;
do_del_nconns([], #network{}) -> true.

%%
%% Delete conns
%%
do_del_conns([C|Es], NN) ->
    case ets:lookup(NN#network.ctab, C) of
    [{C,V1,V2,_}] ->
        do_del_conn(C,V1,V2,NN),
        do_del_conns(Es, NN);
    [] ->
        do_del_conns(Es, NN)
    end;
do_del_conns([], #network{}) -> true.

do_del_conn(C, V1, V2, NN) ->
    ets:select_delete(NN#network.ntab, [{{{in, V2}, C}, [], [true]},
                       {{{out,V1}, C}, [], [true]}]),
    ets:delete(NN#network.ctab, C).

-spec rm_conns([neuron(),...], network()) -> 'true'.

rm_conns([V1, V2|Vs], NN) ->
    rm_conn(V1, V2, NN),
    rm_conns([V2|Vs], NN);
rm_conns(_, _) -> true.

-spec rm_conn(neuron(), neuron(), network()) -> 'ok'.

rm_conn(V1, V2, NN) ->
    Es = out_conns(NN, V1),
    rm_conn_0(Es, V1, V2, NN).
    
rm_conn_0([C|Es], V1, V2, NN) ->
    case ets:lookup(NN#network.ctab, C) of
    [{C, V1, V2, _}]  ->
            do_del_conn(C, V1, V2, NN),
        rm_conn_0(Es, V1, V2, NN);
    _ ->
        rm_conn_0(Es, V1, V2, NN)
    end;
rm_conn_0([], _, _, #network{}) -> ok.
    
%%
%% Check that endpoints exist
%%
-spec do_add_conn({conn(), neuron(), neuron(), label()}, network()) ->
    conn() | {'error', add_conn_err_rsn()}.

do_add_conn({C, V1, V2, Label}, NN) ->
    case ets:member(NN#network.vtab, V1) of
    false -> {error, {bad_neuron, V1}};
    true  ->
        case ets:member(NN#network.vtab, V2) of
        false -> {error, {bad_neuron, V2}};
                true ->
                    case other_conn_exists(NN, C, V1, V2) of
                        true -> {error, {bad_conn, [V1, V2]}};
                        false when NN#network.recurrent =:= false ->
                            acyclic_add_conn(C, V1, V2, Label, NN);
                        false ->
                            do_insert_conn(C, V1, V2, Label, NN)
                    end
        end
    end.

other_conn_exists(#network{ctab = ET}, C, V1, V2) ->
    case ets:lookup(ET, C) of
        [{C, Vert1, Vert2, _}] when Vert1 =/= V1; Vert2 =/= V2 ->
            true;
        _ ->
            false
    end.

-spec do_insert_conn(conn(), neuron(), neuron(), label(), network()) -> conn().

do_insert_conn(C, V1, V2, Label, #network{ntab=NT, ctab=ET}) ->
    ets:insert(NT, [{{out, V1}, C}, {{in, V2}, C}]),
    ets:insert(ET, {C, V1, V2, Label}),
    C.

-spec acyclic_add_conn(conn(), neuron(), neuron(), label(), network()) ->
    conn() | {'error', {'bad_conn', [neuron()]}}.

acyclic_add_conn(_E, V1, V2, _L, _G) when V1 =:= V2 ->
    {error, {bad_conn, [V1, V2]}};
acyclic_add_conn(C, V1, V2, Label, NN) ->
    case get_path(NN, V2, V1) of
    false -> do_insert_conn(C, V1, V2, Label, NN);
    Path -> {error, {bad_conn, Path}}
    end.

%%
%% Delete all paths from neuron V1 to neuron V2
%%

-spec del_path(NN, V1, V2) -> 'true' when
      NN :: network(),
      V1 :: neuron(),
      V2 :: neuron().

del_path(NN, V1, V2) ->
    case get_path(NN, V1, V2) of
    false -> true;
    Path ->
        rm_conns(Path, NN),
        del_path(NN, V1, V2)
    end.

%%
%% Find a cycle through V
%% return the cycle as list of neurons [V ... V]
%% if no cycle exists false is returned
%% if only a cycle of length one exists it will be
%% returned as [V] but only after longer cycles have
%% been searched.
%%

-spec get_cycle(NN, V) -> Neurons | 'false' when
      NN :: network(),
      V :: neuron(),
      Neurons :: [neuron(),...].

get_cycle(NN, V) ->
    case one_path(out_neighbours(NN, V), V, [], [V], [V], 2, NN, 1) of
    false ->
        case lists:member(V, out_neighbours(NN, V)) of
        true -> [V];
        false -> false
        end;
    Vs -> Vs
    end.

%%
%% Find a path from V1 to V2
%% return the path as list of neurons [V1 ... V2]
%% if no path exists false is returned
%%

-spec get_path(NN, V1, V2) -> Neurons | 'false' when
      NN :: network(),
      V1 :: neuron(),
      V2 :: neuron(),
      Neurons :: [neuron(),...].

get_path(NN, V1, V2) ->
    one_path(out_neighbours(NN, V1), V2, [], [V1], [V1], 1, NN, 1).

%%
%% prune_short_path (evaluate conditions on path)
%% short : if path is too short
%% ok    : if path is ok
%%
prune_short_path(Counter, Min) when Counter < Min ->
    short;
prune_short_path(_Counter, _Min) ->
    ok.

one_path([W|Ws], W, Cont, Xs, Ps, Prune, NN, Counter) ->
    case prune_short_path(Counter, Prune) of
    short -> one_path(Ws, W, Cont, Xs, Ps, Prune, NN, Counter);
    ok -> lists:reverse([W|Ps])
    end;
one_path([V|Vs], W, Cont, Xs, Ps, Prune, NN, Counter) ->
    case lists:member(V, Xs) of
    true ->  one_path(Vs, W, Cont, Xs, Ps, Prune, NN, Counter);
    false -> one_path(out_neighbours(NN, V), W, 
              [{Vs,Ps} | Cont], [V|Xs], [V|Ps], 
              Prune, NN, Counter+1)
    end;
one_path([], W, [{Vs,Ps}|Cont], Xs, _, Prune, NN, Counter) ->
    one_path(Vs, W, Cont, Xs, Ps, Prune, NN, Counter-1);
one_path([], _, [], _, _, _, _, _Counter) -> false.

%%
%% Like get_cycle/2, but a cycle of length one is preferred.
%%

-spec get_short_cycle(NN, V) -> Neurons | 'false' when
      NN :: network(),
      V :: neuron(),
      Neurons :: [neuron(),...].

get_short_cycle(NN, V) ->
    get_short_path(NN, V, V).

%%
%% Like get_path/3, but using a breadth-first search makes it possible
%% to find a short path.
%%

-spec get_short_path(NN, V1, V2) -> Neurons | 'false' when
      NN :: network(),
      V1 :: neuron(),
      V2 :: neuron(),
      Neurons :: [neuron(),...].

get_short_path(NN, V1, V2) ->
    T = new(),
    add_neuron(T, V1),
    Q = queue:new(),
    Q1 = queue_out_neighbours(V1, NN, Q),
    L = spath(Q1, NN, V2, T),
    delete(T),
    L.
    
spath(Q, NN, Sink, T) ->
    case queue:out(Q) of
    {{value, C}, Q1} ->
        {_E, V1, V2, _Label} = conn(NN, C),
        if 
        Sink =:= V2 ->
            follow_path(V1, T, [V2]);
        true ->
            case neuron(T, V2) of
            false ->
                add_neuron(T, V2),
                add_conn(T, V2, V1),
                NQ = queue_out_neighbours(V2, NN, Q1),
                spath(NQ, NN, Sink, T);
            _V ->
                spath(Q1, NN, Sink, T)
            end
        end;
    {empty, _Q1} ->
        false
    end.

follow_path(V, T, P) ->
    P1 = [V | P],
    case out_neighbours(T, V) of
    [NN] ->
        follow_path(NN, T, P1);
    [] ->
        P1
    end.

queue_out_neighbours(V, NN, Q0) ->
    lists:foldl(fun(C, Q) -> queue:in(C, Q) end, Q0, out_conns(NN, V)).
