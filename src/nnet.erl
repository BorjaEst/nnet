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
-export([new/0, from_model/1, edit/2, clone/1, concat/1, delete/1]).
-export([start_tables/0, info/1, inputs/1, outputs/1, all_networks/0]).
%% Transactions to run inside 'fun edit/2'
-export([edit_nnode/3]).
-export([connect/2, connect_seq/2, connect_rcc/2, disconnect/2,
         disconnect_allowed/2, move/3, move_allowed/3, 
         reset_weights/2]).
-export([copy/2, clone/2, double/2, split/2, delete/2, merge/2]).
-export([add_input/2, add_output/2]).
%% Exported types
-export_type([id/0, nnode/0, link/0, info/0, model/0]).

-type id()        :: {network, reference()}.
-type network()   :: network:network().
-type nnode()     :: nnode:id().
-type link()      :: {From::nnode(), To::nnode()}.
-type result(Res) :: {'atomic', Res} | {aborted, Reason::term()}.
-type info()      :: network:info().
-type model()     :: model:model().


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
%% @doc Creates an empty new network and returns its id.
%% @end
%%-------------------------------------------------------------------
-spec new() -> result(Id::id()).
new() -> 
    NNET = network:new(),
    ok = mnesia:dirty_write(NNET),
    {network, network:key(NNET)}. 

%%-------------------------------------------------------------------
%% @doc Creates a new network from a model and returns its id.
%% Layers "input" and "output" are mandatory.
%% @end
%%-------------------------------------------------------------------
-spec from_model(Model::model()) -> result(Id::id()).
from_model(Model) -> 
    mnesia:transaction(
        fun() -> 
            NNET = model:compile(Model),
            ok   = mnesia:write(NNET),
            {network, network:key(NNET)}
        end
    ).

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
            NNodes = network:nnodes(NN),
            NMap    = map_copy(NNodes),
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
-spec inputs(Id::id()) -> NNodes::[nnode()].
inputs(Id) -> 
    [NN] = mnesia:dirty_read(Id),
    network:in_nodes(NN).

%%-------------------------------------------------------------------
%% @doc Returns the network outputs.
%% @end
%%-------------------------------------------------------------------
-spec outputs(Id::id()) -> NNodes::[nnode()].
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
%%% API: NNode modifications
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Reinitialises the nnode bias.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec edit_nnode(Ns, NNET, Data) -> NNET when 
    NNET :: network(),
    Ns   :: [nnode()],
    Data :: #{term() => term()}.
edit_nnode(NNodes, NNET, Data) -> 
    [nnode:edit(N, Data) || N <- NNodes],
    NNET.


%%%===================================================================
%%% API: Connections modifications 
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Adds links selecting automatically the nature.
%% By default it implements a sequential, but creates recurrent in 
%% case a dead lock is detected.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec connect(Links, NNET_0) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network().
connect(Links, NNET) -> 
    lists:foldl(fun add_allowed_link/2, NNET, Links).

add_allowed_link({N1,N2}, NN) ->
    case network:seq_path({N2, N1}, NN) of 
        not_found -> network:add_link({N1,N2}, seq, NN);
        _Path     -> network:add_link({N1,N2}, rcc, NN)
    end.

%%-------------------------------------------------------------------
%% @doc Adds links as sequential to the network.
%% If not used correctly might create dead locks, use connect/2 if
%% not sure if a link would create a dead lock.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec connect_seq(Links, NNET_0) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network().
connect_seq(Links, NNET) -> 
    Add_SeqLink = fun(L,NN) -> network:add_link(L, seq, NN) end,
    lists:foldl(Add_SeqLink, NNET, Links).

%%-------------------------------------------------------------------
%% @doc Adds links as recurrent to the network.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec connect_rcc(Links, NNET_0) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network().
connect_rcc(Links, NNET) -> 
    Add_RccLink = fun(L,NN) -> network:add_link(L, rcc, NN) end,
    lists:foldl(Add_RccLink, NNET, Links).

%%-------------------------------------------------------------------
%% @doc Removes links. If the link did not existed nothing happens.
%% Note it might break the network, to do not break it use 
%% disconnect_allowed/2 (Safe but slower).
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec disconnect(Links, NNET_0) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network().
disconnect(Links, NNET) -> 
    lists:foldl(fun network:del_link/2, NNET, [link:delete(L) || L <- Links]). 

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
%% @doc Moves the links using a map.
%% Note it might break the network, to do not break it use 
%% move_allowed/2 (Safe but slower).
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec move(Links, NNET_0, #{Old => New}) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network(),
    Old     :: nnode:id(),
    New     :: nnode:id().
move(Links, NNET, NMap) -> 
    lists:foldl(fun network:move_link/2, NNET, [link:move(L,NMap) || L <- Links]). 

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
    NNET_1 = network:del_link({N1,N2}, NNET_0),
    case path_from_start(N2, NNET_1) of 
        false -> move_allowed(Lx, NNET_0, NMap);
        true  -> 
    case path_to_end(N1, NNET_1) of
        false -> move_allowed(Lx, NNET_0, NMap);
        true  -> 
    link:move({N1,N2}, NMap),
    NNET_2 = case network:seq_path({N2,N1}, NNET_1) of 
        not_found -> network:move_link({N1,N2}, NNET_0, NMap);
        _Path     -> network:add_link({N1,N2}, rcc, NNET_1)
    end,
    move_allowed(Lx, NNET_2, NMap)
    end end;
move_allowed([], NNET, _) -> NNET.

%%-------------------------------------------------------------------
%% @doc Reinitialises the weights of a nnode.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec reset_weights(Links, NNET_0) -> NNET_1 when 
    Links   :: [link()],
    NNET_0  :: network(),
    NNET_1  :: network().
reset_weights(Links, NNET) -> 
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
-spec copy(NNodes, NNET_0) -> NNET_1 when 
    NNodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
copy(NNodes, NNET) -> 
    NMap = map_copy(NNodes),
    Links = links(NNodes, NNET),
    NNET_1 = lists:foldl(fun network:add_nnode/2, NNET, maps:values(NMap)),
    copy_links(Links, NNET_1, NMap).

%%-------------------------------------------------------------------
%% @doc Copies the nnodes, together with the link values.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec clone(NNodes, NNET_0) -> NNET_1 when 
    NNodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
clone(NNodes, NNET) -> 
    NMap = map_copy(NNodes),
    Links = links(NNodes, NNET),
    [link:copy(L,NMap) || L <- Links],
    NNET_1 = lists:foldl(fun network:add_nnode/2, NNET, maps:values(NMap)),
    copy_links(Links, NNET_1, NMap).


%%-------------------------------------------------------------------
%% @doc Clones the nnodes, but new and old links are redeuced 50%. 
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec double(NNodes, NNET_0) -> NNET_1 when 
    NNodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
double(NNodes, NNET) -> 
    NMap = map_copy(NNodes),
    Links = links(NNodes, NNET),
    [link:divide(L,NMap) || L <- Links],
    NNET_1 = lists:foldl(fun network:add_nnode/2, NNET, maps:values(NMap)),
    copy_links(Links, NNET_1, NMap). 

%%-------------------------------------------------------------------
%% @doc Copies the nnodes and distributes links (equal probability). 
%% The original network must have at least 2 inputs and 2 outputs. 
%% The result nnodes will have at least 1 input and 1 output.
%% Should run inside a network edit.
%% @end
%%-------------------------------------------------------------------
-spec split(NNodes, NNET_0) -> NNET_1 when 
    NNodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
split(NNodes, NNET) -> 
    NMap = map_copy(NNodes),
    tbd,
    NNET.

%%-------------------------------------------------------------------
%% @doc Deletes a nnode together with all its connections and links.
%% This action encadenates to all nnodes without a path to the start
%% or end.
%% Should run inside a network edit.
%% @end
%%------------------------------------------------------------------- 
-spec delete(NNodes, NNET_0) -> NNET_1 when 
    NNodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
delete(NNET_0, [N1|Nx]) -> 
    NNET_1 = reset_weights(NNET_0, N1),
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
-spec merge(NNodes, NNET_0) -> NNET_1 when 
    NNodes :: [nnode()],
    NNET_0  :: network(),
    NNET_1  :: network().
merge(NNET, [N1,N2|Nx]) -> 
    % NMap = #{N1 => N2},
    % Lx = [link:merge(Link, NMap) || Link <- network:links(N1, NNET)],
    % merge(delete(connect_allowed(Lx, NNET), [N1]), [N2|Nx]);
    tbd;
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
    connect_seq(
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
    connect_seq(
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
map_copy(NNodes) -> map_copy(NNodes, #{}).

map_copy([N|Nx], NMap) -> map_copy(Nx, NMap#{N => nnode:copy(N)});
map_copy(    [], NMap) -> NMap. 

% Returns a unique list of all nnodes links ------------------------
links(NNodes, NNET) -> 
    lists:usort([L || N <- NNodes, L <- network:links(N, NNET)]).

% Copies the links in the network -----------------------------------
copy_links(Links, NNET, NMap) -> 
    Copy_Link = fun(L,NN) -> network:copy_link(L, NN, NMap) end,
    lists:foldl(Copy_Link, NNET, Links). 

% Returns a non empty/full list of random elements ------------------
random_split([A,B]) -> 
    case rand:uniform() of 
        X when X < 0.5 -> [A];
        _              -> [B]
    end;
random_split(Links) when is_list(Links) -> 
    case ltools:rand(Links, 0-5) of 
        []    -> random_split(Links); 
        Links -> random_split(Links); 
        Other -> _Result = Other           
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

