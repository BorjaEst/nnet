%%%-------------------------------------------------------------------
%%% File    : network_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(network_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(HEAD, [$- || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text), ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$_ || _ <-lists:seq(1,80)]).
-define(END(V), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [V]), V).

-define(INFO(A,B),    ct:log(?LOW_IMPORTANCE,    "~p: ~p",   [A,B])).
-define(ERROR(Error), ct:pal( ?HI_IMPORTANCE, "Error: ~p", [Error])).

-define(SEQ_NETWORK_MAP, #{start => [a1, b1],
                           a1    => [a2, b2],
                           b1    => [a2, b2],
                           a2    => ['end'],
                           b2    => ['end'],
                           'end' => []}).
-define(RCC_NETWORK_MAP, #{start => [x1, y1],
                           x1    => [x2],
                           y1    => [y2],
                           x2    => ['end', y1],
                           y2    => ['end', x1],
                           'end' => []}).



%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 5}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [ ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [
        test_from_to_map,
        test_no_links,
        test_get_links,
        test_add_links,
        test_del_links,
        test_no_nnodes,
        test_get_nnodes,
        test_add_nodes,
        test_del_nodes,
        test_concat_networks

    ].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
my_test_case_example() ->
    [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
my_test_case_example(_Config) ->
    ok.

% --------------------------------------------------------------------
% TESTS --------------------------------------------------------------

% -------------------------------------------------------------------
test_from_to_map(_Config) -> 
    ?HEAD("Correct serialization/deserialisation of network/map .."),
    ok = correct_deserialization(?SEQ_NETWORK_MAP),
    ok = correct_deserialization(?RCC_NETWORK_MAP),
    ok = correct_serialization(seq_network()),
    ok = correct_serialization(rcc_network()),
    ?END(ok).

% -------------------------------------------------------------------
test_no_links(_Config) -> 
    ?HEAD("Correct return of the connections size ................"),
    NN0 = seq_network(),
    Expected_Size = lists:sum(
        [length(Cn) || Cn <- maps:values(?SEQ_NETWORK_MAP)]
    ),
    ok = are_connections_size(Expected_Size, NN0),
    ?END(ok).

% -------------------------------------------------------------------
test_get_links(_Config) -> 
    ?HEAD("Correct collection of the network links ..............."),
    NN0 = seq_network(),
    Expected_Links = 
        [{F,T} || {F,Cn} <- maps:to_list(?SEQ_NETWORK_MAP), T <- Cn],
    ok = are_network_links(Expected_Links, NN0),
    ?END(ok).

% -------------------------------------------------------------------
test_add_links(_Config) ->
    ?HEAD("Correct addition of links in a network ................"),
    NN0 = seq_network(),
    Link = {a1,'end'},
    ok = is_not_in_network(Link, NN0),
    NN1 = network:add_link(Link, NN0),
    ?INFO("Link added to network: ", Link),
    ok = is_in_network(Link, NN1),
    ?END(ok).

% -------------------------------------------------------------------
test_del_links(_Config) ->
    ?HEAD("Correct delete of links in a network .................."),
    NN0 = seq_network(),
    Link = ltools:randnth(network:links(NN0)),
    ok = is_in_network(Link, NN0),
    NN1 = network:del_link(Link, NN0),
    ?INFO("Link removed from network: ", Link),
    ok = is_not_in_network(Link, NN1),
    ?END(ok).

% -------------------------------------------------------------------
test_no_nnodes(_Config) -> 
    ?HEAD("Correct return of the network size ...................."),
    NN0 = seq_network(),
    Expected_Size = maps:size(?SEQ_NETWORK_MAP) - 2,
    ok = is_network_size(Expected_Size, NN0),
    ?END(ok).

% -------------------------------------------------------------------
test_get_nnodes(_Config) -> 
    ?HEAD("Correct collection of the network nnodes ............."),
    NN0 = seq_network(),
    Expected_Nnodes = maps:keys(?SEQ_NETWORK_MAP) -- [start,'end'],
    ok = are_network_nnodes(Expected_Nnodes, NN0),
    ?END(ok).

% -------------------------------------------------------------------
test_add_nodes(_Config) ->
    ?HEAD("Correct addition of nnodes in a network .............."),
    NN0 = seq_network(),
    Node = n1,
    ok = is_not_in_network(Node, NN0),
    NN1 = network:add_nnode(Node, NN0),
    ?INFO("Node added to network: ", Node),
    ok = is_in_network(Node, NN1),
    ?END(ok).

% -------------------------------------------------------------------
test_del_nodes(_Config) ->
    ?HEAD("Correct delete of nnodes in a network ................"),
    NN0 = seq_network(),
    Node  = ltools:randnth(network:nnodes(NN0)),
    Links = network:links(Node, NN0), 
    [ok = is_in_network(X, NN0) || X <- [Node|Links]],
    NN1 = network:del_nnode(Node, NN0),
    ?INFO("Node removed from network: ", Node),
    [ok = is_not_in_network(X, NN1) || X <- [Node|Links]],
    ?END(ok).

% -------------------------------------------------------------------
test_concat_networks(_Config) -> 
    ?HEAD("Correct concatenation of networks ....................."),
    NN1 = seq_network(),
    NN2 = rcc_network(),
    NN3 = network:concat(NN1, NN2),
    ?INFO("Networks concatenated: ", NN3),
    Expected_Nnodes = network:nnodes(NN1) ++ network:nnodes(NN2),
    ok = are_network_nnodes(Expected_Nnodes, NN3),
    L_NN1 = [{F,T} || {F,T} <- network:links(NN1), T =/= 'end'],
    L_NN2 = [{F,T} || {F,T} <- network:links(NN2), F =/= start],
    L_New = [{F,T} || F <- network:out_nodes(NN1), T <- network:in_nodes(NN2)],
    Expected_Links = lists:append([L_NN1, L_NN2, L_New]),
    ok = are_network_links(Expected_Links, NN3),
    ?END(ok).


% --------------------------------------------------------------------
% INDIVIDUAL TEST FUNCTIONS ------------------------------------------

% Checks the correct network serialization --------------------------
correct_serialization(NNET) -> 
    ?HEAD("Is network correclty serialized?"),
    ?INFO("Network: ", NNET),
    Map = network:to_map(NNET),
    ?INFO("Map: ", Map),
    Map = network:to_map(network:from_map(Map)), % Id doesn't matches
    ?END(ok).

% Checks the correct network deserialisation ------------------------
correct_deserialization(Map) -> 
    ?HEAD("Is network correclty deserialized?"),
    ?INFO("Map: ", Map),
    NNET = network:from_map(Map),
    ?INFO("Network: ", NNET), 
    Map = network:to_map(NNET),
    ?END(ok).

% Checks the size of the connections is the indicated ---------------
are_connections_size(N, NNET) -> 
    ?HEAD("Is network connections size correct?"),
    ?INFO("Expected size: ", N),
    ?INFO("Amount of network connections: ", network:no_links(NNET)),
    N = network:no_links(NNET),
    #{connections:=N} = network:info(NNET),
    ?END(ok).

% Checks the links in the network are the indicated -----------------
are_network_links(Links, NNET) -> 
    ?HEAD("Are network links correct?"),
    ?INFO("Expected links: ", Links),
    ?INFO("Network links: ", network:links(NNET)),
    SortedLinks = lists:sort(Links),
    SortedLinks = lists:sort(network:links(NNET)),
    ?END(ok).

% Checks the size of the network is the indicated -------------------
is_network_size(N, NNET) -> 
    ?HEAD("Is network size correct?"),
    ?INFO("Expected size: ", N),
    ?INFO("Network size: ", network:no_nnodes(NNET)),
    N = network:no_nnodes(NNET),
    #{size:=N} = network:info(NNET),
    ?END(ok).

% Checks the network nnodes are the indicated ----------------------
are_network_nnodes(Nnodes, NNET) -> 
    ?HEAD("Are network nnodes correct?"),
    ?INFO("Expected nnodes: ", Nnodes),
    ?INFO("Network nnodes: ", network:nnodes(NNET)),
    SortedNnodes = lists:sort(Nnodes),
    SortedNnodes = lists:sort(network:nnodes(NNET)),
    ?END(ok).

% Checks the link is in the network ---------------------------------
is_in_network({_,_} = Link, NNET) -> 
    ?HEAD("Is link in network?"),
    ?INFO("Link: ", Link),
    ?INFO("Network links: ", network:links(NNET)),
    [_] = [L || L <- network:links(NNET), L == Link], 
    ?END(ok);

% Checks the nnode is in the network -------------------------------
is_in_network(Nnode, NNET) -> 
    ?HEAD("Is nnode in network?"),
    ?INFO("Nnode: ", Nnode),
    ?INFO("Network nnodes : ", network:nnodes(NNET)),
    [_] = [N || N <- network:nnodes(NNET), N == Nnode], 
    ?END(ok).

% Checks the link is NOT in the network -----------------------------
is_not_in_network({_,_} = Link, NNET) -> 
    ?HEAD("Is not link in network?"),
    ?INFO("Link: ", Link),
    ?INFO("Network links: ", network:links(NNET)),
    [] = [L || L <- network:links(NNET), L == Link], 
    ?END(ok);

% Checks the nnode is NOT in the network ---------------------------
is_not_in_network(Nnode, NNET) -> 
    ?HEAD("Is not nnode in network?"),
    ?INFO("Nnode: ", Nnode),
    ?INFO("Network : ", network:nnodes(NNET)),
    [] = [N || N <- network:nnodes(NNET), N == Nnode], 
    ?END(ok).


% -------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS -----------------------------------------

% Creates a simple sequential network -------------------------------
seq_network() -> 
    network:from_map(?SEQ_NETWORK_MAP).

% Creates a simple recurrent network --------------------------------
rcc_network() -> 
    network:from_map(?RCC_NETWORK_MAP).

