%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% -TODO: Check if it is better to act as well on the bias.
%%% -TODO: Might be interesting to test in "divide_nnode" to test 
%%%        dividing only the inputs or only the outputs.
%%% @end
%%%-------------------------------------------------------------------
-module(nnet).

%% API
-export([start_tables/0, new/2, edit/2, clone/1, concat/1, delete/1]).
-export([info/1, inputs/1, outputs/1, all_networks/0]).
%% Transactions to run inside 'fun edit/2'
-export([reinitialise_bias/2, switch_activation/3]).
-export([connect_all/2,     disconnect_all/2,     move_all/3,
         connect_allowed/2, disconnect_allowed/2, move_allowed/3,
         delete_weights/2]).
-export([copy/2, clone/2, double/2, split/2, delete/2, merge/2]).
-export([add_input/2, add_output/2]).
%% Exported types
-export_type([id/0, nnode/0, link/0, info/0]).

-type id()        :: {network, reference()}.
-type network()   :: network:network().
-type nnode()    :: nnode:id().
-type link()      :: {From::nnode(), To::nnode()}.
-type result(Res) :: {'atomic', Res} | {aborted, Reason::term()}.
-type info()      :: network:info().

%%%===================================================================
%%% Main API functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Creates a new network and returns its id.
%% @end
%%-------------------------------------------------------------------
-spec start_tables() -> ok.
start_tables() ->
    true = new_table(network, network:record_fields()),
    true = new_table(   link,     [  from_to, weight]),
    true = new_table( nnode,     [reference,   data]),
    ok.

%%-------------------------------------------------------------------
%% @doc Creates a new network and returns its id.
%% @end
%%-------------------------------------------------------------------
-spec new(Map, Properties) -> result(Id::id()) when 
    Map        :: #{Node => ConnectionsTo::[Node]},
    Properties :: nnode:properties(),
    Node       :: atom() | 'start'.
new(Map, Properties) -> 
    mnesia:transaction(
        fun() ->
            RMap = create_nodes(Map, Properties),
            NMap = rename(Map, RMap),
            NNET = network:from_map(NMap),
            [ok = check_nnode(N, NNET) || N <- network:nnodes(NNET)],
            ok   = mnesia:write(NNET),
            {network, network:key(NNET)}
        end
    ). 

create_nodes(Map, P) -> map_nodes(maps:keys(Map), #{}, P).

map_nodes([start|Rx], RMap, P) -> map_nodes(Rx, RMap#{start => start}, P);
map_nodes(['end'|Rx], RMap, P) -> map_nodes(Rx, RMap#{'end' => 'end'}, P);
map_nodes([    X|Rx], RMap, P) -> map_nodes(Rx, RMap#{X=>nnode:new(P)}, P);
map_nodes(        [], RMap, _) -> RMap. 

rename(Map, RMap) when is_map(Map) -> 
    maps:from_list(
        [{rename(K,RMap),rename(V,RMap)} || {K,V}<-maps:to_list(Map)]
    );
rename(List, RMap) when is_list(List) -> 
    [rename(Elem, RMap) || Elem <- List];
rename(Key, RMap) -> 
    map_get(Key, RMap).


%%-------------------------------------------------------------------
%% @doc Performs in a transaction the operations defined inside the 
%% passed function. This function must be of arity one where the 
%% passed argument is the network itself (not the id) which is always 
%% the first argument or return of the following functions.
%% @end
%%-------------------------------------------------------------------
-spec edit(Id, Function) -> result(ok) when
    Id       :: id(),
    Function :: function().
edit(Id, Function) -> 
    mnesia:transaction(
        fun() -> 
            [NNET_0] = mnesia:wread(Id),
             NNET_1  = apply(Function, [NNET_0]),
             ok      = mnesia:write(NNET_1)
        end
    ).

%%-------------------------------------------------------------------
%% @doc Clones a network.
%% @end
%%-------------------------------------------------------------------
-spec clone(Id::id()) -> result(Clone::id()).
clone(Id) -> 
    mnesia:transaction(
        fun() -> 
            [NN]    = mnesia:read(Id),
            Nnodes = network:nnodes(NN),
            NMap    = map_copy(Nnodes),
            Clone = network:rename(network:copy(NN), NMap),
            [link:copy({From,To}, NMap) || {From,To} <- network:links(NN)],
            ok = mnesia:write(Clone),
            {network, network:key(Clone)}
        end
    ).

%%-------------------------------------------------------------------
%% @doc Concatenates a list of networks connecting each network 
%% output to all inputs of the next network in the list.
%% The merged networks are deleted in the process but not its links
%% and nnodes, which are only moved to the new network.
%% @end
%%-------------------------------------------------------------------
-spec concat(Ids) -> result(Concatenated) when 
    Ids          :: [id()],
    Concatenated :: id().
concat(Ids) -> 
    mnesia:transaction(
        fun() -> 
            NNx  = [hd(mnesia:read(Id)) || Id <- Ids],
            Concatenated = concat_nnet(NNx),
            [ok = mnesia:delete(Id) || Id <- Ids],
            ok = mnesia:write(Concatenated),
            {network, network:key(Concatenated)}
        end
    ).

concat_nnet([NNET_1, NNET_2 | Networks]) -> 
    NNET_12 = network:concat(NNET_1, NNET_2),
    concat_nnet([NNET_12 | Networks]);
concat_nnet([NNET]) -> 
    NNET.

%%-------------------------------------------------------------------
%% @doc Deletes a network.
%% @end
%%-------------------------------------------------------------------
-spec delete(Id::id()) -> result(ok).
delete(Id) -> 
    mnesia:transaction(
        fun() -> 
            [NN] = mnesia:read(Id),
            delete(NN, network:in_nodes(NN)), % Encadenates all
            ok = mnesia:delete(Id)
        end
    ).

%%-------------------------------------------------------------------
%% @doc Returns network information.
%% @end
%%-------------------------------------------------------------------
-spec info(Id::id()) -> info().
info(Id) -> 
    [NN] = mnesia:dirty_read(Id),
    network:info(NN).

%%-------------------------------------------------------------------
%% @doc Returns the network inputs.
%% @end
%%-------------------------------------------------------------------
-spec inputs(Id::id()) -> Nnodes::[nnode()].
inputs(Id) -> 
    [NN] = mnesia:dirty_read(Id),
    network:in_nodes(NN).

%%-------------------------------------------------------------------
%% @doc Returns the network outputs.
%% @end
%%-------------------------------------------------------------------
-spec outputs(Id::id()) -> Nnodes::[nnode()].
outputs(Id) -> 
    [NN] = mnesia:dirty_read(Id),
    network:out_nodes(NN).

%%-------------------------------------------------------------------
%% @doc Returns a list with all networks.
%% @end
%%-------------------------------------------------------------------
-spec all_networks() -> Ids::[id()].
all_networks() -> 
    mnesia:all_keys(network).


%%%===================================================================
%%% API: Nnode modifications
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Reinitialises the nnode bias.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec reinitialise_bias(Ns, NNET) -> NNET when 
    NNET :: network(),
    Ns   :: [nnode()].
reinitialise_bias(Nnodes, NNET) -> 
    [nnode:edit(N, #{bias=>undefined}) || N <- Nnodes],
    NNET.

%%-------------------------------------------------------------------
%% @doc Changes the activation function of a nnode.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec switch_activation(Ns, NNET, Function_name) -> NNET when 
    NNET :: network(),
    Ns   :: nnode(),
    Function_name :: atom().
switch_activation(Nnodes, NNET, Function_name) -> 
    [nnode:edit(N, #{activation=>Function_name}) || N <- Nnodes],
    NNET.


%%%===================================================================
%%% API: Connections modifications 
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Creates links from all nnodes in From to all nnodes in To.
%% If the link existed alreay, it is not modified.
%% Note it might break the network, to do not break it use 
%% connect_allowed/2 (Safe but slower).
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec connect_all(Links, NNET_0) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network().
connect_all(Links, NNET) -> 
    lists:foldl(fun network:add_link/2, NNET, Links).

%%-------------------------------------------------------------------
%% @doc Deletes links from all nnodes in From to all nnodes in To.
%% If the link did not existed nothing happens.
%% Note it might break the network, to do not break it use 
%% disconnect_allowed/2 (Safe but slower).
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec disconnect_all(Links, NNET_0) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network().
disconnect_all(Links, NNET) -> 
    lists:foldl(fun network:del_link/2, NNET, [link:delete(L) || L <- Links]). 

%%-------------------------------------------------------------------
%% @doc Moves all the links using a map.
%% Note it might break the network, to do not break it use 
%% move_allowed/2 (Safe but slower)..
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec move_all(Links, NNET_0, #{Old => New}) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network(),
    Old     :: nnode:id(),
    New     :: nnode:id().
move_all(Links, NNET, NMap) -> 
    lists:foldl(fun network:move_link/2, NNET, [link:move(L,NMap) || L <- Links]). 

%%-------------------------------------------------------------------
%% @doc Creates only the allowed links from nnodes in From to To.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec connect_allowed(Links, NNET_0) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network().
connect_allowed([L|Lx], NNET_0) -> 
    NNET_1 = network:add_link(L, NNET_0),
    %% TODO: Check different types (seq, rcc, etc)?
    connect_allowed(Lx, NNET_1);
connect_allowed([], NNET) -> NNET.

%%-------------------------------------------------------------------
%% @doc Deletes only the allowed links from all nnodes in From to 
%% all nnodes in To. If the link did not existed nothing happens.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec disconnect_allowed(Links, NNET_0) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network().
disconnect_allowed([{N1,N2}|Lx], NNET_0) -> 
    NNET_1 = network:del_link({N1,N2}, NNET_0),
    case path_from_start(N2, NNET_1) of 
        false -> disconnect_allowed(Lx, NNET_0);
        true  -> 
    case path_to_end(N1, NNET_1) of
        false -> disconnect_allowed(Lx, NNET_0);
        true  -> 
    link:delete({N1,N2}),
    disconnect_allowed(Lx, NNET_1)
    end end;
disconnect_allowed([], NNET) -> NNET. 

%%-------------------------------------------------------------------
%% @doc Moves only the allowed links using a map.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec move_allowed(Links, NNET_0, #{Old => New}) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network(),
    Old     :: nnode:id(),
    New     :: nnode:id().
move_allowed([{N1,N2}|Lx], NNET_0, NMap) -> 
    NNET_1 = network:move_link(NNET_0, {N1,N2}, NMap), 
    case path_from_start(N2, NNET_1) of 
        false -> move_allowed(Lx, NNET_0, NMap);
        true  -> 
    case path_to_end(N1, NNET_1) of
        false -> move_allowed(Lx, NNET_0, NMap);
        true  -> 
    link:move({N1,N2}, NMap),    
    move_allowed(Lx, NNET_1, NMap)
    end end;
move_allowed([], NNET, _) -> NNET.

%%-------------------------------------------------------------------
%% @doc Reinitialises the weights of a nnode.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec delete_weights(Links, NNET_0) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network().
delete_weights(Links, NNET) -> 
    [link:delete(Link) || Link <- Links],
    NNET.


%%%===================================================================
%%% API: Network modifications
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Copies the nnodes, but not the link values.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec copy(Nnodes, NNET_0) -> NNET_1 when 
    Nnodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
copy(Nnodes, NNET) -> 
    NMap = map_copy(Nnodes),
    Links = [link:map(L,NMap) || L <- links(Nnodes, NNET)],
    NNET_1 = lists:foldl(fun network:add_nnode/2, NNET, maps:values(NMap)),
    connect_all(Links, NNET_1). 

%%-------------------------------------------------------------------
%% @doc Copies the nnodes, together with the link values.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec clone(Nnodes, NNET_0) -> NNET_1 when 
    Nnodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
clone(Nnodes, NNET) -> 
    NMap = map_copy(Nnodes),
    Links = [link:copy(L,NMap) || L <- links(Nnodes, NNET)],
    NNET_1 = lists:foldl(fun network:add_nnode/2, NNET, maps:values(NMap)),
    connect_all(Links, NNET_1). 

%%-------------------------------------------------------------------
%% @doc Clones the nnodes, but new and old links are redeuced 50%. 
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec double(Nnodes, NNET_0) -> NNET_1 when 
    Nnodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
double(Nnodes, NNET) -> 
    NMap = map_copy(Nnodes),
    Links = [link:divide(L,NMap) || L <- links(Nnodes, NNET)],
    NNET_1 = lists:foldl(fun network:add_nnode/2, NNET, maps:values(NMap)),
    connect_all(Links, NNET_1). 

%%-------------------------------------------------------------------
%% @doc Copies the nnodes and distributes links (equal probability). 
%% The original network must have at least 2 inputs and 2 outputs. 
%% The result nnodes will have at least 1 input and 1 output.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec split(Nnodes, NNET_0) -> NNET_1 when 
    Nnodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
split(Nnodes, NNET) -> 
    NMap = map_copy(Nnodes),
    % ExternalLinksIn  = [{F,N} || N <- Nnodes,
    %                              F <- network:in_nodes(NNET, N),
    %                              not is_map_key(F, NMap)],
    % ExternalLinksOut = [{N,T} || N <- Nnodes,
    %                              T <- network:out_nodes(NNET, N),
    %                              not is_map_key(T, NMap)],
    % InternalLinks    = [{F,N} || N <- Nnodes,
    %                              F <- network:in_nodes(NNET, N),
    %                              is_map_key(F, NMap)],
    Links = [link:move(L,NMap) || L <- links(Nnodes, Nnodes), 
                                  0.5 > rand:uniform()],                                                
    NNET_1 = move_all(Links, NNET, NMap),
    [ok = check_nnode(NNET_1, N) || N <- Nnodes],
    NNET_1.

%%-------------------------------------------------------------------
%% @doc Deletes a nnode together with all its connections and links.
%% This action encadenates to all nnodes without a path to the start
%% or end.
%% Should run inside a network edit.
%% @end
%%------------------------------------------------------------------- 
-spec delete(Nnodes, NNET_0) -> NNET_1 when 
    Nnodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
delete(NNET_0, [N1|Nx]) -> 
    NNET_1 = delete_weights(NNET_0, N1),
    NNET_2 = network:delete(N1, NNET_1),
    NNET_3 = lists:foldl( 
        fun(N, NNET) -> case path_to_end(N, NNET) of 
                            false -> delete(N, NNET);
                            true  -> NNET
                        end
        end, NNET_2, network:in_nodes(N1, NNET_2)),
    NNET_4 = lists:foldl( 
        fun(N, NNET) -> case path_from_start(N, NNET) of 
                            false -> delete(N, NNET);
                            true  -> NNET
                        end
        end, NNET_3, network:out_nodes(N1, NNET_3)),
    delete(NNET_4, Nx) ;
delete([], NNET) -> 
    NNET.

%%-------------------------------------------------------------------
%% @doc Merges all inputs and outputs of all nnodes from left to 
%% right. All nnodes except the last are so deleted.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec merge(Nnodes, NNET_0) -> NNET_1 when 
    Nnodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
merge(NNET, [N1,N2|Nx]) -> 
    NMap = #{N1 => N2},
    Lx = [link:merge(Link, NMap) || Link <- network:links(N1, NNET)],
    merge(delete(connect_allowed(Lx, NNET), [N1]), [N2|Nx]);
merge(NNET, [_]) -> 
    NNET.

%%-------------------------------------------------------------------
%% @doc Adds a nnode as input. Connected nnodes need to be 
%% specified.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec add_input(Neighbours, NNET_0) -> NNET_1 when 
    Neighbours :: [nnode()],
    NNET_0     :: network(),
    NNET_1     :: network().
add_input(Neighbours, NNET_0) ->
    NInput = nnode:new(#{}),
    connect_all(
        network:add_nnode(NInput, NNET_0),
        [{start,NInput}|[{NInput,N2} || N2 <- Neighbours]]
    ). 

%%-------------------------------------------------------------------
%% @doc Adds a nnode as output. Connected nnodes need to be 
%% specified.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec add_output(Neighbours, NNET_0) -> NNET_1 when 
    Neighbours :: [nnode()],
    NNET_0     :: network(),
    NNET_1     :: network().
add_output(Neighbours, NNET_0) ->
    NOutput = nnode:new(#{}),
    connect_all(
        network:add_nnode(NOutput, NNET_0),
        [{NOutput,'end'}|[{N1,NOutput} || N1 <- Neighbours]]
    ). 


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

% Copies the nnodes and retruns a map #{Old=>New} ------------------
map_copy(Nnodes) -> map_copy(Nnodes, #{}).

map_copy([N|Nx], NMap) -> map_copy(Nx, NMap#{N => nnode:copy(N)});
map_copy(    [], NMap) -> NMap. 

% Returns a unique list of all nnodes links ------------------------
links(Nnodes, NNET) -> 
    lists:usort([L || N <- Nnodes, L <- network:links(N, NNET)]).

% Returns a non empty/full list of random elements ------------------
random_split([A,B]) -> 
    case rand:uniform() of 
        X when X < 0.5 -> A;
        _              -> B
    end;
random_split([_,_|_] = Links) -> 
    case ltools:rand(Links, 0.50) of 
        []       -> random_split(Links); % Try again
        Links    -> random_split(Links); % Try again
        RandList -> RandList            % Ok
    end;
random_split(_Other) -> 
    error({badarg, "Request a list of at least 2 inputs/outputs"}).

% Checks there is a path from the start -----------------------------
path_from_start(N, NNET) -> 
    case network:seq_path({start,N}, NNET) of
        not_found -> false;
        _Path     -> true 
    end.

% Checks there is a path to the end ---------------------------------
path_to_end(N, NNET) -> 
    case network:seq_path({N,'end'}, NNET) of
        not_found -> false;
        _Path     -> true 
    end.

% Function that checks for nnode defects --------------------------- 
check_nnode(N, NNET) -> 
    case path_from_start(N, NNET) of
        false -> {no_path, #{start=>N}};
        true  -> 
    case path_to_end(N, NNET) of
        false -> {no_path, #{N=>'end'}};
        true  -> ok 
    end end.


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
