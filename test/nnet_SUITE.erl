%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%% Created :
%%%-------------------------------------------------------------------
-module(nnet_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(HEAD, [$↓ || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text), ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$^ || _ <-lists:seq(1,80)]).
-define(END(V), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [V]), V).

-define(INFO(A,B),    ct:log(?LOW_IMPORTANCE,    "~p: ~p",   [A,B])).
-define(ERROR(Error), ct:pal( ?HI_IMPORTANCE, "Error: ~p", [Error])).
-define(TEST(Fun, Network), nnet:edit(fun() -> Fun(Network) end)).

-define(TEST_MODEL, #{inputs  => #{connections => #{
                                    layer1  => sequential,
                                    layer2  => {sequential,0.5}}, 
                                   units => 2, data=>#{}},
                      layer1  => #{connections => #{
                                    layer2  => sequential,
                                    outputs => {sequential,0.5}}, 
                                   units => 4, data=>#{}},
                      layer2  => #{connections => #{
                                    layer2  => recurrent,
                                    outputs => sequential}, 
                                   units => 4, data=>#{}},
                      outputs => #{connections => #{
                                    inputs  => {recurrent, 0.5},
                                    layer2  => recurrent}, 
                                   units => 2, data=>#{}}}
).


%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 8}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = application:start(mnesia),
    ok = nnet:start_tables(),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok = application:stop(mnesia),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    {atomic, Id} = nnet:from_model(?TEST_MODEL),
    [{network_id, Id} | Config].

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
    [
        {in_sequence_nnode_operations, [sequence, shuffle],
         [
             correct_rnode,
             correct_wnode,
             correct_out_nnodes, 
             correct_out_seq_nnodes,
             correct_out_rcc_nnodes,
             correct_in_nnodes
         ]
        },
        {in_sequence_link_operations, [sequence, shuffle],
         [
             correct_connect_auto,
             correct_connect_seq,
             correct_connect_rcc,
             correct_disconnect,
             correct_move,
             correct_reset
         ]
        },
        {in_sequence_network_operations, [sequence, shuffle],
         [
             correct_nnode_copy,
             correct_nnode_clone,
             correct_nnode_divide,
            %  correct_nnode_split,
            %  correct_nnode_delete,
             correct_nnode_join
            %  correct_make_input,
            %  correct_make_output
         ]
        },
        {in_parallel_operations, [parallel, shuffle],
        %  [here_goes_the_test || _ <- lists:seq(1,5)] ++
         [my_test_case_example]
        }
    ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [ 
        {group, in_sequence_nnode_operations},
        {group, in_sequence_link_operations},
        {group, in_sequence_network_operations},
        {group, in_parallel_operations}
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
correct_rnode(Config) ->
    ?HEAD("Correct rnode .........................................."),
    {atomic, Result} = ?TEST(fun rnode/1, 
                             ?config(network_id, Config)),
    ?END(Result).

rnode(Network) ->
    NNode = random_nnode(Network),
    ?INFO("Writing bias=1.0 in nnode: ", NNode),
    ok = nnode:edit(NNode, #{bias=>1.0}),
    true = is_bias(1.0, NNode),
    ?INFO("Reading bias for nnode: ", NNode),
    #{bias:=Bias} = nnet:rnode(NNode),
    true = is_bias(Bias, NNode),
    ok.

% -------------------------------------------------------------------
correct_wnode(Config) ->
    ?HEAD("Correct wnode .........................................."),
    {atomic, Result} = ?TEST(fun wnode/1, 
                             ?config(network_id, Config)),
    ?END(Result).

wnode(Network) -> 
    NNode = random_nnode(Network),
    ?INFO("Writing bias=1.0 in nnode: ", NNode),
    ok = nnode:edit(NNode, #{bias=>1.0}),
    true = is_bias(1.0, NNode),
    ?INFO("Reinitialising bias for nnode: ", NNode),
    ok = nnet:wnode(NNode, #{bias=>not_init}),
    true = is_bias(not_init, NNode),
    ok.

% -------------------------------------------------------------------
correct_out_seq_nnodes(Config) ->
    ?HEAD("Correct seq nnodes ....................................."),
    {atomic, Result} = ?TEST(fun out_seq_nnodes/1, 
                             ?config(network_id, Config)),
    ?END(Result).

out_seq_nnodes(Network) -> 
    NNode1 = random_nnode(Network),
    NNode2 = random_nnode(Network),
    ?INFO("Setting connection: seq, 2.0 in:", {NNode1,NNode2}),
    ok = link:add({NNode1,NNode2}, seq, 2.0),
    ?INFO("Getting seq links from NNode1: ", NNode1),
    Seq_Links = nnet:out_seq(NNode1),
    ?INFO("Links: ", Seq_Links),
    true = one_in_list({NNode1,NNode2}, Seq_Links),
    ok.

% -------------------------------------------------------------------
correct_out_rcc_nnodes(Config) ->
    ?HEAD("Correct rcc nnodes ....................................."),
    {atomic, Result} = ?TEST(fun out_rcc_nnodes/1, 
                             ?config(network_id, Config)),
    ?END(Result).

out_rcc_nnodes(Network) -> 
    NNode1 = random_nnode(Network),
    NNode2 = random_nnode(Network),
    ?INFO("Setting connection: rcc, 2.0 in:", {NNode1,NNode2}),
    ok = link:add({NNode1,NNode2}, rcc, 2.0),
    ?INFO("Getting rcc links from NNode1: ", NNode1),
    Rcc_Links = nnet:out_rcc(NNode1),
    ?INFO("Links: ", Rcc_Links),
    true = one_in_list({NNode1,NNode2}, Rcc_Links),
    ok.

% -------------------------------------------------------------------
correct_out_nnodes(Config) ->
    ?HEAD("Correct out nnodes ....................................."),
    {atomic, Result} = ?TEST(fun out_nnodes/1, 
                             ?config(network_id, Config)),
    ?END(Result).

out_nnodes(Network) -> 
    NNode1 = random_nnode(Network),
    NNode2 = random_nnode(Network),
    ?INFO("Setting connection: rcc, 2.0 in:", {NNode1,NNode2}),
    ok = link:add({NNode1,NNode2}, seq, 2.0),
    ok = link:add({NNode1,NNode2}, rcc, 2.0),
    ?INFO("Getting rcc links from NNode1: ", NNode1),
    Out_Links = nnet:out(NNode1),
    ?INFO("Links: ", Out_Links),
    true = one_in_list({NNode1,NNode2}, Out_Links),
    ok.

% -------------------------------------------------------------------
correct_in_nnodes(Config) ->
    ?HEAD("Correct in nnodes ....................................."),
    {atomic, Result} = ?TEST(fun in_nnodes/1, 
                             ?config(network_id, Config)),
    ?END(Result).

in_nnodes(Network) -> 
    NNode1 = random_nnode(Network),
    NNode2 = random_nnode(Network),
    ?INFO("Setting connection: rcc, 2.0 in:", {NNode1,NNode2}),
    ok = link:add({NNode1,NNode2}, rcc, 2.0),
    ?INFO("Getting in links from NNode2: ", NNode2),
    In_Links = nnet:in(NNode2),
    ?INFO("Links: ", In_Links),
    true = one_in_list({NNode1,NNode2}, In_Links),
    ok.

% -------------------------------------------------------------------
correct_connect_auto(Config) ->
    ?HEAD("Correct auto connect of NNodes ........................."),
    {atomic, Result} = ?TEST(fun connect_auto/1, 
                             ?config(network_id, Config)),
    ?END(Result).

connect_auto(Network) -> 
    Links = [{F,T} || F <- random_nnodes(Network), 
                      T <- random_nnodes(Network)],
    ?INFO("Removing Links: ", Links),
    ok = nnet:disconnect(Links),
    [false = is_defined(Link) || Link <- Links],
    ?INFO("Connecting links as auto: ", Links),
    ok = nnet:connect(Links),
    [true = is_defined(Link) || Link <- Links],
    ok.

% -------------------------------------------------------------------
correct_connect_seq(Config) ->
    ?HEAD("Correct sequential connect of NNodes .................."),
    {atomic, Result} = ?TEST(fun connect_seq/1, 
                             ?config(network_id, Config)),
    ?END(Result).

connect_seq(Network) -> 
    Links = [{F,T} || F <- random_nnodes(Network), 
                      T <- random_nnodes(Network)],
    ?INFO("Removing Links: ", Links),
    ok = nnet:disconnect(Links),
    [false = is_defined(Link) || Link <- Links],
    ?INFO("Connecting links as sequential: ", Links),
    ok = nnet:connect_seq(Links),
    [true = is_sequential(Link) || Link <- Links],
    ok.

% -------------------------------------------------------------------
correct_connect_rcc(Config) ->
    ?HEAD("Correct recurrent connect of NNodes ..................."),
    {atomic, Result} = ?TEST(fun connect_rcc/1, 
                             ?config(network_id, Config)),
    ?END(Result).

connect_rcc(Network) -> 
    Links = [{F,T} || F <- random_nnodes(Network), 
                      T <- random_nnodes(Network)],
    ?INFO("Removing Links: ", Links),
    ok = nnet:disconnect(Links),
    [false = is_defined(Link) || Link <- Links],
    ?INFO("Connecting links as recurrent: ", Links),
    ok = nnet:connect_rcc(Links),
    [true = is_recurrent(Link) || Link <- Links],
    ok.

% -------------------------------------------------------------------
correct_disconnect(Config) ->
    ?HEAD("Correct disconnection of NNodes ......................."),
    {atomic, Result} = ?TEST(fun disconnect/1, 
                             ?config(network_id, Config)),
    ?END(Result).

disconnect(Network) -> 
    Links = [{F,T} || F <- random_nnodes(Network), 
                      T <- random_nnodes(Network)],
    ?INFO("Re/Connecting Links: ", Links),
    ok = nnet:connect(Links),
    [true = is_defined(Link) || Link <- Links], 
    ?INFO("Disconnecting links: ", Links),
    ok = nnet:disconnect(Links),
    [false = is_defined(Link) || Link <- Links], 
    ok.

% -------------------------------------------------------------------
correct_move(Config) ->
    ?HEAD("Correct move of links ................................."),
    {atomic, Result} = ?TEST(fun move/1, 
                             ?config(network_id, Config)),
    ?END(Result).

move(Network) -> 
    Links = [{F,T} || F <- random_nnodes(Network), 
                      T <- random_nnodes(Network)],            
    NNode1 = random_nnode(Network),
    NNode2 = random_nnode(Network), 
    NMap = #{NNode1 => NNode2},
    PrevDefined = [L || L <- Links, is_defined(L)],
    LW_Node1 = [{L, weight(L)} || L <- Links, is_defined(L)],  
    LW_Node2 = [{map_link(L,NMap), weight(map_link(L,NMap))} 
        || L <- Links, is_defined(L)],  
    ?INFO("Move {NNode1::from() => NNode2::to()}: ", NMap),
    ?INFO("Links & W from Node1 available: ", LW_Node1),
    ?INFO("Links available in Node1 map to Node2 & W: ", LW_Node2),
    ?INFO("Moving Links: ", Links),
    ok = nnet:move(Links, NMap),
    [true = is_defined(map_link(L,NMap)) || L <- PrevDefined],
    [true = is_weight(L2, merge(W1,W2)) 
        || {{_,W1},{L2,W2}} <- lists:zip(LW_Node1, LW_Node2)],
    ok.

% -------------------------------------------------------------------
correct_reset(Config) ->
    ?HEAD("Correct reset of weights .............................."),
    {atomic, Result} = ?TEST(fun reset/1, 
                             ?config(network_id, Config)),
    ?END(Result).

reset(Network) -> 
    Links = [{F,T} || F <- random_nnodes(Network), 
                      T <- random_nnodes(Network)],
    ?INFO("Setting weight=1.0 in: ", Links),
    [ok = link:write(Link, 1.0) || Link <- Links],
    [true =  is_weight(Link, 1.0) || Link <- Links],
    ?INFO("Deleting links weights: ", Links),
    ok = nnet:reset(Links),
    [true = is_weight(Link, not_init) || Link <- Links],
    ok.

% -------------------------------------------------------------------
correct_nnode_copy(Config) ->
    ?HEAD("Correct copy of nnodes .............................."),
    {atomic, Result} = ?TEST(fun nnode_copy/1, 
                                 ?config(network_id, Config)),
    ?END(Result).

nnode_copy(Network) ->
    NNode1 = random_nnode(Network),
    ?INFO("Getting in/outputs from: ", NNode1),
    {In_N1, Out_N1} = {in_nodes(NNode1), out_nodes(NNode1)},
    ?INFO("In/Out NNode1: ", {In_N1, Out_N1}),
    Network_size = network:size(Network),
    Links = nnet:out(NNode1) ++ nnet:in(NNode1), 
    ?INFO("Setting weight=1.0 in: ", Links),
    [ok = link:write(Link, 1.0) || Link <- Links],
    ?INFO("Copying node: ", NNode1),
    NNode2 = nnet:copy(NNode1, Network),
    ?INFO("Getting in/outputs from copy: ", NNode2),
    {In_N2, Out_N2} = {in_nodes(NNode2), out_nodes(NNode2)},
    ?INFO("In/Out NNode2: ", {In_N2, Out_N2}),
    true = has_the_elements( replace(In_N1, NNode1, NNode2),  In_N2),
    true = has_the_elements(replace(Out_N1, NNode1, NNode2), Out_N2),
    [true = is_weight({N,NNode2}, not_init) || N <-  In_N2],
    [true = is_weight({NNode2,N}, not_init) || N <- Out_N2],
    true = is_network_size(Network, Network_size+1),
    ok.

% -------------------------------------------------------------------
correct_nnode_clone(Config) ->
    ?HEAD("Correct clone of nnodes .............................."),
    {atomic, Result} = ?TEST(fun nnode_clone/1, 
                                 ?config(network_id, Config)),
    ?END(Result).

nnode_clone(Network) ->
    NNode1 = random_nnode(Network),
    ?INFO("Getting in/outputs from: ", NNode1),
    {In_N1, Out_N1} = {in_nodes(NNode1), out_nodes(NNode1)},
    ?INFO("In/Out NNode1: ", {In_N1, Out_N1}),
    Network_size = network:size(Network),
    Links = nnet:out(NNode1) ++ nnet:in(NNode1), 
    ?INFO("Setting weight=1.0 in: ", Links),
    [ok = link:write(Link, 1.0) || Link <- Links],
    ?INFO("Cloning node: ", NNode1),
    NNode2 = nnet:clone(NNode1, Network),
    ?INFO("Getting in/outputs from copy: ", NNode2),
    {In_N2, Out_N2} = {in_nodes(NNode2), out_nodes(NNode2)},
    ?INFO("In/Out NNode2: ", {In_N2, Out_N2}),
    true = has_the_elements( replace(In_N1, NNode1, NNode2),  In_N2),
    true = has_the_elements(replace(Out_N1, NNode1, NNode2), Out_N2),
    [true = is_weight({N,NNode2}, 1.0) || N <-  In_N2],
    [true = is_weight({NNode2,N}, 1.0) || N <- Out_N2],
    true = is_network_size(Network, Network_size+1),
    ok.

% -------------------------------------------------------------------
correct_nnode_divide(Config) ->
    ?HEAD("Correct divide of nnodes .............................."),
    {atomic, Result} = ?TEST(fun nnode_divide/1, 
                                 ?config(network_id, Config)),
    ?END(Result).

nnode_divide(Network) ->
    NNode1 = random_nnode(Network),
    ?INFO("Getting in/outputs from: ", NNode1),
    {In_N1, Out_N1} = {in_nodes(NNode1), out_nodes(NNode1)},
    ?INFO("In/Out NNode1: ", {In_N1, Out_N1}),
    Network_size = network:size(Network),
    Links = nnet:out(NNode1) ++ nnet:in(NNode1), 
    ?INFO("Setting weight=1.0 in: ", Links),
    [ok = link:write(Link, 1.0) || Link <- Links],
    ?INFO("Cloning node: ", NNode1),
    NNode2 = nnet:divide(NNode1, Network),
    ?INFO("Getting in/outputs from copy: ", NNode2),
    {In_N2, Out_N2} = {in_nodes(NNode2), out_nodes(NNode2)},
    ?INFO("In/Out NNode2: ", {In_N2, Out_N2}),
    true = has_the_elements( replace(In_N1, NNode1, NNode2),  In_N2),
    true = has_the_elements(replace(Out_N1, NNode1, NNode2), Out_N2),
    [true = is_weight(Link, 0.5)     || Link <- Links],
    [true = is_weight({N,NNode2}, 0.5) || N <-  In_N2],
    [true = is_weight({NNode2,N}, 0.5) || N <- Out_N2],
    true = is_network_size(Network, Network_size+1),
    ok.

% -------------------------------------------------------------------
correct_nnode_join(Config) ->
    ?HEAD("Correct join of nnodes .............................."),
    {atomic, Result} = ?TEST(fun nnode_join/1, 
                                 ?config(network_id, Config)),
    ?END(Result).

nnode_join(Network) ->
    NNode1 = random_nnode(Network),
    NNode2 = random_nnode(Network),
    ?INFO("Getting in/outputs from: ", NNode1),
    {In_N1, Out_N1} = {in_nodes(NNode1), out_nodes(NNode1)},
    ?INFO("In/Out NNode1: ", {In_N1, Out_N1}),
    ?INFO("Getting in/outputs from: ", NNode2),
    {In_N2, Out_N2} = {in_nodes(NNode2), out_nodes(NNode2)},
    ?INFO("In/Out NNode2: ", {In_N2, Out_N2}),
    Network_size = network:size(Network),
    ?INFO("Merging nodes: ", {NNode1, NNode2}),
    ok = nnet:join({NNode1,NNode2}, Network),
    {In_N2x, Out_N2x} = {in_nodes(NNode2), out_nodes(NNode2)},
    ?INFO("In/Out merged NNode2: ", {In_N2x, Out_N2x}),
    [true = one_in_list(N,  In_N2x) || N <-  In_N1 ++  In_N2, N=/=NNode1],
    [true = one_in_list(N, Out_N2x) || N <- Out_N1 ++ Out_N2, N=/=NNode1],
    case NNode1==NNode2 of 
        true  -> true = is_network_size(Network, Network_size);
        false -> true = is_network_size(Network, Network_size-1)
    end,
    ok.


% --------------------------------------------------------------------
% INDIVIDUAL TEST FUNCTIONS ------------------------------------------

% Checks the value of the bias is the expected ----------------------
is_bias(Expected, NNode) -> 
    ?HEAD("Is bias the expected value?"),
    #{bias:=Bias} = nnode:read(NNode),
    ?INFO("Expected: ", Expected),
    ?INFO("Bias: ", Bias),
    ?END(Expected == Bias).

% Checks the list contains the element once -------------------------
one_in_list(Element, List) -> 
    ?HEAD("Is the Element 1 time in the List?"),
    ?INFO("Element: ", Element),
    ?INFO("List: ", List),
    ?END([Element] == [X || X <- List, Element =:= X]).

% Checks the list contains all the elements -------------------------
has_the_elements(Elements, List) -> 
    ?HEAD("Has the list all the elements?"),
    ?INFO("List: ", List),
    ?INFO("Elements: ", Elements),
    ?END([] == Elements -- List).

% Checks the link value is the expected -----------------------------
is_weight(Link, Expected) -> 
    ?HEAD("Is the link weight correct?"),
    Weight = link:read(Link),
    ?INFO("Link: ", Link),
    ?INFO("Expected: ", Expected),
    ?INFO("Weight: ", Weight),
    ?END(Expected == Weight).  

% Checks the size of the network is the expected ------
is_network_size(Network, Expected) -> 
    ?HEAD("Is the network size the expected?"),
    Size = network:size(Network),
    ?INFO("Expected: ", Expected),
    ?INFO("Size: ", Size),
    ?END(Expected == Size).

% Checks the link is existing ---------------------------------------
is_defined({_,To} = Link) -> 
    ?HEAD("Is the link existing?"),
    ?INFO("Link: ", Link),
    ?END(lists:any(fun(X) -> X=:=Link end, nnet:in(To))).

% Checks the link exists and it is sequential -----------------------
is_sequential({From,_} = Link) -> 
    ?HEAD("Is the link existing and sequential?"),
    ?INFO("Link: ", Link),
    ?END(lists:any(fun(X) -> X=:=Link end, nnet:out_seq(From))).

% Checks the link exists and it is recurrent ------------------------
is_recurrent({From,_} = Link) -> 
    ?HEAD("Is the link existing and recurrent?"),
    ?INFO("Link: ", Link),
    ?END(lists:any(fun(X) -> X=:=Link end, nnet:out_rcc(From))).


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% Returns a random nnode from the network --------------------------
random_nnode(Network) ->
    NNodes = [N || {N,node} <- maps:to_list(network:nnodes(Network))],
    ltools:randnth(NNodes).

% Return random nnodes from the network ----------------------------
random_nnodes(Network) -> 
    NNodes = [N || {N,node} <- maps:to_list(network:nnodes(Network))],
    case ltools:rand(NNodes, 0.35) of 
        []       -> random_nnodes(Network); % Try again
        NNodes   -> random_nnodes(Network); % Try again
        RandList -> RandList              % Ok
    end.

% Returns the links which belong to the network ---------------------
intersection(Links, Network) -> 
    sets:to_list(
        sets:intersection(
            sets:from_list(Links), 
            sets:from_list(network:links(Network))
    )).

% Returns the in nodes ----------------------------------------------
in_nodes(NNode) -> 
    [From || {From,_} <- nnet:in(NNode)].

% Returns the out nodes ---------------------------------------------
out_nodes(NNode) -> 
    [To || {_,To} <- nnet:out(NNode)].

% Maps a link from a tuple {From,To} --------------------------------
map_link({From, To}, Map) ->
    {maps:get(From, Map, From), maps:get(To, Map, To)}.

% Returns link weight replacing exception not_defined by atom -------
weight(Link) ->
    try link:read(Link) of 
        Value -> Value
    catch
        error:{{link, Link}, not_defined} -> not_defined
    end.

% Merges to weight values -------------------------------------------
merge(not_defined,          W2) ->    W2; 
merge(         W1, not_defined) -> W1;  
merge(   not_init,          W2) ->    W2;  
merge(         W1,    not_init) -> W1;  
merge(         W1,          W2) -> W1+W2.

% Replaces all elements of a list -----------------------------------
replace(List, Elem, By) -> 
    Replace = 
        fun(X) when X==Elem -> By;
           (X)              -> X
        end,
    lists:map(Replace, List).


% --------------------------------------------------------------------
% RESULTS CONSOLE PRINT ----------------------------------------------

