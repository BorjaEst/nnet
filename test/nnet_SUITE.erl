%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%% Created :
%%%-------------------------------------------------------------------
-module(nnet_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(HEAD, [$- || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text), ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$_ || _ <-lists:seq(1,80)]).
-define(END(V), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [V]), V).

-define(INFO(A,B),    ct:log(?LOW_IMPORTANCE,    "~p: ~p",   [A,B])).
-define(ERROR(Error), ct:pal( ?HI_IMPORTANCE, "Error: ~p", [Error])).

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
            %  correct_rnode,
             correct_wnode,
            %  correct_out_nnodes,
            %  correct_out_seq_nnodes,
            %  correct_out_rcc_nnodes,
             correct_in_nnodes
         ]
        },
        {in_sequence_link_operations, [sequence, shuffle],
         [
            %  correct_connect_auto,
            %  correct_connect_seq,
            %  correct_connect_rcc,
            %  correct_disconnect,
            %  correct_move,
            %  correct_merge
             correct_reset
         ]
        },
        {in_sequence_network_operations, [sequence, shuffle],
         [
            %  correct_nnode_copy,
            %  correct_nnode_clone,
            %  correct_nnode_divide,
            %  correct_nnode_split,
            %  correct_nnode_delete,
             correct_nnode_join
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
correct_wnode(Config) ->
    ?HEAD("Correct wnode .........................................."),
    Network = ?config(network_id, Config),
    Test = 
        fun() -> 
            NNode = random_nnode(Network),
            ?INFO("Writing bias=1.0 in nnode: ", NNode),
            ok = nnode:edit(NNode, #{bias=>1.0}),
            ok = is_bias(1.0, NNode),
            ?INFO("Reinitialising bias for nnode: ", NNode),
            ok = nnet:wnode(NNode, #{bias=>not_init}),
            ok = is_bias(not_init, NNode)
        end,
    {atomic, Result} = nnet:edit(Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_in_nnodes(Config) ->
    ?HEAD("Correct in nnodes ....................................."),
    Network = ?config(network_id, Config),
    Test = 
        fun() -> 
            NNode1 = random_nnode(Network),
            NNode2 = random_nnode(Network),
            ?INFO("Setting connection: rcc, 2.0 in:", {NNode1,NNode2}),
            ok = link:add({NNode1,NNode2}, rcc, 2.0),
            ?INFO("Getting in links from NNode2: ", NNode2),
            In_Links = nnet:in(NNode2),
            ok = one_in_list({NNode1,NNode2}, In_Links)
        end,
    {atomic, Result} = nnet:edit(Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_reset(Config) ->
    ?HEAD("Correct reset of weights .............................."),
    Network = ?config(network_id, Config),   
    Test = 
        fun() -> 
            Links = [{F,T} || F <- random_nnodes(Network), 
                              T <- random_nnodes(Network)],
            ?INFO("Setting weight=1.0 in: ", Links),
            [ok = link:write(Link, 1.0) || Link <- Links],
            [ok =  is_weight(Link, 1.0) || Link <- Links],
            ?INFO("Deleting links weights: ", Links),
             ok = nnet:reset(Links),
            [ok = is_weight(Link, not_init) || Link <- Links],
             ok
        end,
    {atomic, Result} = nnet:edit(Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_nnode_join(Config) ->
    ?HEAD("Correct join of nnodes .............................."),
    Network = ?config(network_id, Config),
    Test = 
        fun() -> 
            NNode1 = random_nnode(Network),
            NNode2 = random_nnode(Network),
            ?INFO("Getting in/outputs from: ", NNode1),
            {In_N1, Out_N1} = {in_nodes(NNode1), out_nodes(NNode1)},
            ?INFO("In/Out Nnode1: ", {In_N1, Out_N1}),
            ?INFO("Getting in/outputs from: ", NNode2),
            {In_N2, Out_N2} = {in_nodes(NNode2), out_nodes(NNode2)},
            ?INFO("In/Out Nnode2: ", {In_N2, Out_N2}),
            Network_size = network:size(Network),
            ?INFO("Merging nodes: ", {NNode1, NNode2}),
             ok = nnet:join({NNode1,NNode2}, Network),
            {In_N2x, Out_N2x} = {in_nodes(NNode2), out_nodes(NNode2)},
            ?INFO("In/Out merged Nnode2: ", {In_N2x, Out_N2x}),
            [ok = one_in_list(N,  In_N2x) || N <-  In_N1 ++  In_N2, N=/=NNode1],
            [ok = one_in_list(N, Out_N2x) || N <- Out_N1 ++ Out_N2, N=/=NNode1],
            case NNode1==NNode2 of 
                true  -> ok = is_network_size(Network, Network_size);
                false -> ok = is_network_size(Network, Network_size-1)
            end
        end,
    {atomic, Result} = nnet:edit(Test),
    ?END(Result).


% --------------------------------------------------------------------
% INDIVIDUAL TEST FUNCTIONS ------------------------------------------

% Checks the value of the bias is the expected ----------------------
is_bias(Expected, NNode) -> 
    ?HEAD("Is bias the expected value?"),
    #{bias:=Bias} = nnode:read(NNode),
    ?INFO("Expected: ", Expected),
    ?INFO("Bias: ", Bias),
    Expected = Bias,
    ?END(ok).

% Checks the list contains the element once -------------------------
one_in_list(Element, List) -> 
    ?HEAD("Is the Element 1 time in the List?"),
    ?INFO("Element: ", Element),
    ?INFO("List: ", List),
    [Element] = [X || X <- List, Element =:= X],
    ?END(ok).

% Checks the link value is the expected -----------------------------
is_weight(Link, Expected) -> 
    ?HEAD("Is the link weight correct?"),
    Weight = link:read(Link),
    ?INFO("Link: ", Link),
    ?INFO("Expected: ", Expected),
    ?INFO("Weight: ", Weight),
    Expected = Weight,
    ?END(ok).  

% Checks the size of the network is the expected ------
is_network_size(Network, Expected) -> 
    ?HEAD("Is the network size the expected?"),
    Size = network:size(Network),
    ?INFO("Expected: ", Expected),
    ?INFO("Size: ", Size),
    Expected = Size,
    ?END(ok).


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% Returns a random nnode from the network --------------------------
random_nnode(Network) ->
    NNodes = [N || {N,node} <- maps:to_list(network:nnodes(Network))],
    ltools:randnth(NNodes).

% Return random nnodes from the network ----------------------------
random_nnodes(Network) -> 
    NNodes = [N || {N,node} <- maps:to_list(network:nnodes(Network))],
    case ltools:rand(NNodes, 0.75) of 
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


% --------------------------------------------------------------------
% RESULTS CONSOLE PRINT ----------------------------------------------

