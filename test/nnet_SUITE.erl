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

-define(NETWORK_MAP, #{start => [x1, y1],
                       x1    => [x2],
                       y1    => [y2],
                       x2    => ['end', y1],
                       y2    => ['end', x1],
                       'end' => []}).
-define(NNODE_PROPERTIES, #{activation  => direct,
                             aggregation => direct,
                             initializer => ones}).


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
    {atomic, Id} = nnet:new(?NETWORK_MAP, ?NNODE_PROPERTIES),
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
        {in_sequence_nnode_edits, [sequence, shuffle],
         [
             correct_bias_reinitialisation,
             correct_switch_activation
         ]
        },
        {in_sequence_connections_edits, [sequence, shuffle],
         [
            %  correct_connect_all,
             correct_connect_allowed,
             correct_disconnect_all,
             correct_disconnect_allowed,
            %  correct_move_all,
            %  correct_move_allowed,
             correct_delete_of_weights
         ]
        },
        {in_sequence_network_edits, [sequence, shuffle],
         [
             correct_nnode_copy,
             correct_nnode_clone,
             correct_nnode_double
         ]
        },
        {in_parallel_edits, [parallel, shuffle],
         [correct_bias_reinitialisation     || _ <- lists:seq(1,5)] ++ 
         [correct_switch_activation         || _ <- lists:seq(1,5)] ++
         %  [correct_connect_all               || _ <- lists:seq(1,5)] ++        
         [correct_connect_allowed           || _ <- lists:seq(1,5)] ++
         [correct_disconnect_all            || _ <- lists:seq(1,5)] ++
         [correct_disconnect_allowed        || _ <- lists:seq(1,5)] ++
        %  [correct_move_all                  || _ <- lists:seq(1,5)] ++
        %  [correct_move_allowed              || _ <- lists:seq(1,5)] ++
         [correct_delete_of_weights         || _ <- lists:seq(1,5)] ++
         [correct_nnode_copy               || _ <- lists:seq(1,5)] ++
         [correct_nnode_clone              || _ <- lists:seq(1,5)] ++
         [correct_nnode_double             || _ <- lists:seq(1,5)] ++
         []
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
        {group, in_sequence_nnode_edits},
        {group, in_sequence_connections_edits},
        {group, in_sequence_network_edits}
        % {group, in_parallel_edits}
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
correct_bias_reinitialisation(Config) ->
    ?HEAD("Correct bias reinitialisation ........................."),
    Test = 
        fun(NNET_0) -> 
            Nnode = random_nnode(NNET_0),
            nnode:edit(Nnode, #{bias=>1.0}),
            ok = is_bias_initialised(Nnode),
            ?INFO("reinitialising bias for nnode: ", Nnode),
            NNET_1 = nnet:reinitialise_bias([Nnode], NNET_0),
            ok = is_bias_not_initialised(Nnode),
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_switch_activation(Config) ->
    ?HEAD("Correct activation switch ............................."),
    Test = 
        fun(NNET_0) -> 
            Nnode = random_nnode(NNET_0),
            nnode:edit(Nnode, #{activation=>direct}),
            ok = is_activation(direct, Nnode),
            ?INFO("Switching activation to: ", {Nnode, elu}),
            NNET_1 = nnet:switch_activation([Nnode], NNET_0, elu),
            ok = is_activation(elu, Nnode),
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_connect_allowed(Config) -> 
    ?HEAD("Correct connection of nnodes when allowed ............"),
    Test = 
        fun(NNET_0) -> 
            Links = [{F,T} || F<-random_nnodes(NNET_0), T<-random_nnodes(NNET_0)],
            ?INFO("Connecting allowed links of: ", Links),
            NNET_1 = nnet:connect_allowed(Links, NNET_0),
            Expected_links = Links,
            [ok = network_SUITE:is_in_network(X, NNET_1) || X <- Expected_links],
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_disconnect_all(Config) -> 
    ?HEAD("Correct disconnection of allowed without error ........"),
    Test = 
        fun(NNET_0) -> 
            Links = [{F,T} || F<-random_nnodes(NNET_0), T<-random_nnodes(NNET_0)],
            ?INFO("Disconnecting all: ", Links),
            NNET_1 = nnet:disconnect_all(Links, NNET_0),
            [ok = network_SUITE:is_not_in_network(X, NNET_1) || X <- Links],
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result). 

% -------------------------------------------------------------------
correct_disconnect_allowed(Config) -> 
    ?HEAD("Correct disconnection of nnodes when allowed ........."),
    Test = 
        fun(NNET_0) -> 
            Links = [{F,T} || F<-random_nnodes(NNET_0), T<-random_nnodes(NNET_0)],
            ?INFO("Disconnecting allowed links of: ", Links),
            NNET_1 = nnet:disconnect_allowed(Links, NNET_0),
            RemainingLinks = intersection(Links, NNET_1),
            ?INFO("Remaining links: ", RemainingLinks),
            [ok = is_not_erasable(L, NNET_1)|| L <- RemainingLinks],
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_delete_of_weights(Config) ->
    ?HEAD("Correct delete of weights ............................."),
    Test = 
        fun(NNET_0) -> 
            Links = [{F,T} || F<-random_nnodes(NNET_0), T<-random_nnodes(NNET_0)],
            [link:write(Link, 1.0) || Link <- Links],
            [ok = is_link_initialised(Link) || Link <- Links],
            ?INFO("Deleting links values: ", Links),
            NNET_1 = nnet:delete_weights(Links, NNET_0),
            [ok = is_link_weight(undefined, Link) || Link <- Links],
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_nnode_copy(Config) ->
    ?HEAD("Correct nnode copy ..................................."),
    Test = 
        fun(NNET_0) -> 
            Nnodes = random_nnodes(NNET_0),
            ?INFO("Copying the nnodes: ", Nnodes),
            NNET_1 = nnet:copy(Nnodes, NNET_0),
            ok = has_network_increased(length(Nnodes), NNET_1, NNET_0), 
            Copies = network:nnodes(NNET_1) -- network:nnodes(NNET_0),
            ?INFO("New from copy nnodes: ", Copies),
            [ok = is_link_weight(undefined, L) || N <- Copies, L <- network:links(N,NNET_1)],
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_nnode_clone(Config) ->
    ?HEAD("Correct nnode clone .................................."),
    Test = 
        fun(NNET_0) -> 
            Nnodes = random_nnodes(NNET_0),
            [link:write(L,-1.0) || N <- Nnodes, L <- network:links(N,NNET_0)],
            ?INFO("Cloning the nnodes: ", Nnodes),
            NNET_1 = nnet:clone(Nnodes, NNET_0),
            ok = has_network_increased(length(Nnodes), NNET_1, NNET_0), 
            Clones = network:nnodes(NNET_1) -- network:nnodes(NNET_0),
            ?INFO("New from clone nnodes: ", Clones),
            [ok = is_link_weight(-1.0, L) || N <- Clones, L <- network:links(N,NNET_1)],
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_nnode_double(Config) ->
    ?HEAD("Correct nnode double ................................."),
    Test = 
        fun(NNET_0) -> 
            Nnodes = random_nnodes(NNET_0),
            [link:write(L,2.0) || N <- Nnodes, L <- network:links(N,NNET_0)],
            ?INFO("Doubling the nnodes: ", Nnodes),
            NNET_1 = nnet:double(Nnodes, NNET_0),
            ok = has_network_increased(length(Nnodes), NNET_1, NNET_0), 
            Doubled = network:nnodes(NNET_1) -- network:nnodes(NNET_0),
            ?INFO("New from doubled nnodes: ", Doubled),
            [ok = is_link_weight(2.0,L) || N      <- Doubled, 
                                          {_,T}=L <- network:links(N,NNET_1), 
                                          lists:member(T,Doubled)],
            [ok = is_link_weight(1.0,L) || N      <- Doubled, 
                                          {_,T}=L <- network:links(N,NNET_1), 
                                          not lists:member(T,Doubled)], 
            [ok = is_link_weight(2.0,L) || N      <- Nnodes, 
                                          {_,T}=L <- network:links(N,NNET_1), 
                                          lists:member(T,Nnodes)],
            [ok = is_link_weight(1.0,L) || N      <- Nnodes, 
                                          {_,T}=L <- network:links(N,NNET_1), 
                                          not lists:member(T,Nnodes)], 
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).


% --------------------------------------------------------------------
% INDIVIDUAL TEST FUNCTIONS ------------------------------------------

% Checks the value of the bias is NOT undefined ---------------------
is_bias_initialised(Nnode) -> 
    ?HEAD("Is bias initialised?"),
    #{bias:=Bias} = nnode:read(Nnode),
    ?INFO("Bias: ", Bias),
    true = is_float(Bias),
    ?END(ok).

% Checks the value of the bias is undefined -------------------------
is_bias_not_initialised(Nnode) -> 
    ?HEAD("Is bias initialised?"),
    #{bias:=Bias} = nnode:read(Nnode),
    ?INFO("Bias: ", Bias),
    undefined = Bias,
    ?END(ok).

% Checks the activation matches with the nnode activation ----------
is_activation(Activation, Nnode) -> 
    ?HEAD("Is nnode activation correct?"),
    #{activation:=Func} = nnode:read(Nnode),
    ?INFO("{activation:, expected:} ", {Activation, Func}),
    Func = Activation,
    ?END(ok).

% Checks that the network is on the first path between From->To -----
is_in_path({From,To}, NNET, Nnode) -> 
    ?HEAD("Is nnode member of the path?"),
    Path = network:seq_path({From,To}, NNET),
    ?INFO("Nnode: ", Nnode),
    ?INFO("Path: ", Path),
    true = lists:member(Nnode, Path),
    ?END(ok). 

% Checks that the network is on the first path between From->To -----
is_not_erasable({From,To}, NNET_0) -> 
    ?HEAD("Is nnode broken if link deleted?"),
    NNET_1 = network:del_link({From,To}, NNET_0),
    PathStartToTo = network:seq_path({start,To}, NNET_1),
    ?INFO("Path from start->to if deleted: ",{{start,To}, PathStartToTo}),
    PathFromToEnd = network:seq_path({From,'end'}, NNET_1),
    ?INFO("Path from from->end if deleted: ", {{From,'end'}, PathFromToEnd}),
    ?INFO("At least one should be: ", not_found),
    true = (not_found==PathStartToTo) or (not_found==PathFromToEnd),
    ?END(ok). 

% Checks the links of both networks are the same --------------------
have_the_same_links(NNET_1, NNET_2) -> 
    ?HEAD("Does this 2 networks have the same links?"),
    OrderedLinks1 = lists:sort(network:links(NNET_1)),
    OrderedLinks2 = lists:sort(network:links(NNET_2)),
    ?INFO("Links network 1: ", NNET_1),
    ?INFO("Links network 2: ", NNET_2),
    OrderedLinks1 = OrderedLinks2,
    ?END(ok).  

% Checks the value of the link is not undefined ---------------------
is_link_initialised(Link) -> 
    ?HEAD("Is the link weight defined (a float)?"),
    Weight = link:read(Link),
    ?INFO("{Link, weight}: ", {Link, Weight}),
    true = is_float(Weight),
    ?END(ok). 

% Checks the link value is the specified ---------------------
is_link_weight(Expected, Link) -> 
    ?HEAD("Is the link weight correct?"),
    Weight = link:read(Link),
    ?INFO("Link: ", Link),
    ?INFO("Expected: ", Expected),
    ?INFO("Weight: ", Weight),
    Expected = Weight,
    ?END(ok).  

% Checks the number of nnodes increased in the defined number ------
has_network_increased(Expected, NNET_1, NNET_0) -> 
    ?HEAD("Has the network increased in the specified value?"),
    Size_0 = network:no_nnodes(NNET_0),
    Size_1 = network:no_nnodes(NNET_1),
    ?INFO("{Size before, Size after}: ", {Size_0, Size_1}),
    ?INFO("Expected increment: ", Expected),
    Expected = Size_1 - Size_0,
    ?END(ok).


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% Returns a random nnode from the network --------------------------
random_nnode(NNET) -> 
    ltools:randnth(network:nnodes(NNET)).

% Return random nnodes from the network ----------------------------
random_nnodes(NNET) -> 
    Nnodes = network:nnodes(NNET),
    case ltools:rand(Nnodes, 0.75) of 
        []       -> random_nnodes(NNET); % Try again
        Nnodes  -> random_nnodes(NNET); % Try again
        RandList -> RandList              % Ok
    end.

% Returns the links intersenction -----------------------------------
intersection(Links, NNET) -> 
    sets:to_list(
        sets:intersection(
            sets:from_list(Links), 
            sets:from_list(network:links(NNET))
    )).


% --------------------------------------------------------------------
% RESULTS CONSOLE PRINT ----------------------------------------------
