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
-define(NEURON_PROPERTIES, #{activation  => direct,
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
    {atomic, Id} = nnet:new(?NETWORK_MAP, ?NEURON_PROPERTIES),
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
        {in_sequence_neuron_edits, [sequence, shuffle],
         [
             correct_bias_reinitialisation,
             correct_switch_activation
         ]
        },
        {in_sequence_connections_edits, [sequence, shuffle],
         [
             correct_connect_allowed,
            %  correct_connect_all,
             correct_disconnect_allowed,
             correct_disconnect_all,
            %  correct_move_allowed,
            %  correct_move_all,
             correct_delete_of_weights
         ]
        },
        {in_sequence_network_edits, [sequence, shuffle],
         [
             correct_neuron_copy,
             correct_neuron_clone,
             correct_neuron_double
         ]
        },
        {in_parallel_edits, [parallel, shuffle],
         [correct_bias_reinitialisation     || _ <- lists:seq(1,5)] ++ 
         [correct_switch_activation         || _ <- lists:seq(1,5)] ++
         [correct_connect_allowed           || _ <- lists:seq(1,5)] ++
        %  [correct_connect_all               || _ <- lists:seq(1,5)] ++
         [correct_disconnect_allowed        || _ <- lists:seq(1,5)] ++
         [correct_disconnect_all            || _ <- lists:seq(1,5)] ++
        %  [correct_move_allowed              || _ <- lists:seq(1,5)] ++
        %  [correct_move_all                  || _ <- lists:seq(1,5)] ++
         [correct_delete_of_weights         || _ <- lists:seq(1,5)] ++
         [correct_neuron_copy               || _ <- lists:seq(1,5)] ++
         [correct_neuron_clone              || _ <- lists:seq(1,5)] ++
         [correct_neuron_double             || _ <- lists:seq(1,5)] ++
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
        {group, in_sequence_neuron_edits},
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
            Neuron = random_neuron(NNET_0),
            neuron:edit(Neuron, #{bias=>1.0}),
            ok = is_bias_initialised(Neuron),
            ?INFO("reinitialising bias for neuron: ", Neuron),
            NNET_1 = nnet:reinitialise_bias([Neuron], NNET_0),
            ok = is_bias_not_initialised(Neuron),
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_switch_activation(Config) ->
    ?HEAD("Correct activation switch ............................."),
    Test = 
        fun(NNET_0) -> 
            Neuron = random_neuron(NNET_0),
            neuron:edit(Neuron, #{activation=>direct}),
            ok = is_activation(direct, Neuron),
            ?INFO("Switching activation to: ", {Neuron, elu}),
            NNET_1 = nnet:switch_activation([Neuron], NNET_0, elu),
            ok = is_activation(elu, Neuron),
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_connect_allowed(Config) -> 
    ?HEAD("Correct connection of neurons when allowed ............"),
    Test = 
        fun(NNET_0) -> 
            Links = [{F,T} || F<-random_neurons(NNET_0), T<-random_neurons(NNET_0)],
            ?INFO("Connecting allowed links of: ", Links),
            NNET_1 = nnet:connect_allowed(Links, NNET_0),
            Expected_links = Links,
            [ok = network_SUITE:is_in_network(X, NNET_1) || X <- Expected_links],
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result). 

% -------------------------------------------------------------------
correct_disconnect_allowed(Config) -> 
    ?HEAD("Correct disconnection of neurons when allowed ........."),
    Test = 
        fun(NNET_0) -> 
            Links = [{F,T} || F<-random_neurons(NNET_0), T<-random_neurons(NNET_0)],
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
correct_disconnect_all(Config) -> 
    ?HEAD("Disconnection of not allowed raises error ............."),
    Test1 = 
        fun(NNET_0) -> 
            Links = [{F,T} || F<-random_neurons(NNET_0), T<-random_neurons(NNET_0)],
            NNET_1 = nnet:disconnect_allowed(Links, NNET_0),
            NotAllowedLinks = intersection(Links, NNET_1),
            [ok = returns_disconnection_error([L], NNET_1) || L <- NotAllowedLinks],
            NNET_0
        end,
    {atomic, Result1} = nnet:edit(?config(network_id, Config), Test1),
    ?HEAD("Correct disconnection of allowed without error ........"),
    Test2 = 
        fun(NNET_0) -> 
            Links = [{F,T} || F<-random_neurons(NNET_0), T<-random_neurons(NNET_0)],
            NNET_1 = nnet:disconnect_allowed(Links, NNET_0),
            AllowedLinks = network:links(NNET_0) -- network:links(NNET_1),
            ?INFO("Disconnecting allowed links of: ", Links),
            NNET_2 = nnet:disconnect_allowed(AllowedLinks, NNET_0),
            ok = have_the_same_links(NNET_1, NNET_2),
            NNET_2
        end,
    {atomic, Result2} = nnet:edit(?config(network_id, Config), Test2),
    ?END({Result1, Result2}). 

% -------------------------------------------------------------------
correct_delete_of_weights(Config) ->
    ?HEAD("Correct delete of weights ............................."),
    Test = 
        fun(NNET_0) -> 
            Links = [{F,T} || F<-random_neurons(NNET_0), T<-random_neurons(NNET_0)],
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
correct_neuron_copy(Config) ->
    ?HEAD("Correct neuron copy ..................................."),
    Test = 
        fun(NNET_0) -> 
            Neurons = random_neurons(NNET_0),
            ?INFO("Copying the neurons: ", Neurons),
            NNET_1 = nnet:copy(Neurons, NNET_0),
            ok = has_network_increased(length(Neurons), NNET_1, NNET_0), 
            Copies = network:neurons(NNET_1) -- network:neurons(NNET_0),
            ?INFO("New from copy neurons: ", Copies),
            [ok = is_link_weight(undefined, L) || N <- Copies, L <- network:links(N,NNET_1)],
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_neuron_clone(Config) ->
    ?HEAD("Correct neuron clone .................................."),
    Test = 
        fun(NNET_0) -> 
            Neurons = random_neurons(NNET_0),
            [link:write(L,-1.0) || N <- Neurons, L <- network:links(N,NNET_0)],
            ?INFO("Cloning the neurons: ", Neurons),
            NNET_1 = nnet:clone(Neurons, NNET_0),
            ok = has_network_increased(length(Neurons), NNET_1, NNET_0), 
            Clones = network:neurons(NNET_1) -- network:neurons(NNET_0),
            ?INFO("New from clone neurons: ", Clones),
            [ok = is_link_weight(-1.0, L) || N <- Clones, L <- network:links(N,NNET_1)],
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).

% -------------------------------------------------------------------
correct_neuron_double(Config) ->
    ?HEAD("Correct neuron double ................................."),
    Test = 
        fun(NNET_0) -> 
            Neurons = random_neurons(NNET_0),
            [link:write(L,2.0) || N <- Neurons, L <- network:links(N,NNET_0)],
            ?INFO("Doubling the neurons: ", Neurons),
            NNET_1 = nnet:double(Neurons, NNET_0),
            ok = has_network_increased(length(Neurons), NNET_1, NNET_0), 
            Doubled = network:neurons(NNET_1) -- network:neurons(NNET_0),
            ?INFO("New from doubled neurons: ", Doubled),
            [ok = is_link_weight(2.0,L) || N      <- Doubled, 
                                          {_,T}=L <- network:links(N,NNET_1), 
                                          lists:member(T,Doubled)],
            [ok = is_link_weight(1.0,L) || N      <- Doubled, 
                                          {_,T}=L <- network:links(N,NNET_1), 
                                          not lists:member(T,Doubled)], 
            [ok = is_link_weight(2.0,L) || N      <- Neurons, 
                                          {_,T}=L <- network:links(N,NNET_1), 
                                          lists:member(T,Neurons)],
            [ok = is_link_weight(1.0,L) || N      <- Neurons, 
                                          {_,T}=L <- network:links(N,NNET_1), 
                                          not lists:member(T,Neurons)], 
            NNET_1
        end,
    {atomic, Result} = nnet:edit(?config(network_id, Config), Test),
    ?END(Result).


















% --------------------------------------------------------------------
% INDIVIDUAL TEST FUNCTIONS ------------------------------------------

% Checks the value of the bias is NOT undefined ---------------------
is_bias_initialised(Neuron) -> 
    ?HEAD("Is bias initialised?"),
    #{bias:=Bias} = neuron:read(Neuron),
    ?INFO("Bias: ", Bias),
    true = is_float(Bias),
    ?END(ok).

% Checks the value of the bias is undefined -------------------------
is_bias_not_initialised(Neuron) -> 
    ?HEAD("Is bias initialised?"),
    #{bias:=Bias} = neuron:read(Neuron),
    ?INFO("Bias: ", Bias),
    undefined = Bias,
    ?END(ok).

% Checks the activation matches with the neuron activation ----------
is_activation(Activation, Neuron) -> 
    ?HEAD("Is neuron activation correct?"),
    #{activation:=Func} = neuron:read(Neuron),
    ?INFO("{activation:, expected:} ", {Activation, Func}),
    Func = Activation,
    ?END(ok).

% Checks that the network is on the first path between From->To -----
is_in_path({From,To}, NNET, Neuron) -> 
    ?HEAD("Is neuron member of the path?"),
    Path = network:path({From,To}, NNET),
    ?INFO("Neuron: ", Neuron),
    ?INFO("Path: ", Path),
    true = lists:member(Neuron, Path),
    ?END(ok). 

% Checks that the network is on the first path between From->To -----
is_not_erasable({From,To}, NNET_0) -> 
    ?HEAD("Is neuron broken if link deleted?"),
    NNET_1 = network:del_link({From,To}, NNET_0),
    PathStartToTo = network:path({start,To}, NNET_1),
    ?INFO("Path from start->to if deleted: ",{{start,To}, PathStartToTo}),
    PathFromToEnd = network:path({From,'end'}, NNET_1),
    ?INFO("Path from from->end if deleted: ", {{From,'end'}, PathFromToEnd}),
    ?INFO("At least one should be: ", not_found),
    true = (not_found==PathStartToTo) or (not_found==PathFromToEnd),
    ?END(ok). 

% Checks the disconnection returns an error -------------------------
returns_disconnection_error(Links, NNET) -> 
    ?HEAD("Does the disconnection raises an error?"),
    try nnet:disconnect_all(Links, NNET) of 
        _ -> error("disconnection did not raise error")
    catch error:{no_path, Path} -> 
        ?INFO("Error reason: ", {no_path, Path}),
        ?END(ok)
    end.

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

% Checks the number of neurons increased in the defined number ------
has_network_increased(Expected, NNET_1, NNET_0) -> 
    ?HEAD("Has the network increased in the specified value?"),
    Size_0 = network:no_neurons(NNET_0),
    Size_1 = network:no_neurons(NNET_1),
    ?INFO("{Size before, Size after}: ", {Size_0, Size_1}),
    ?INFO("Expected increment: ", Expected),
    Expected = Size_1 - Size_0,
    ?END(ok).
















% % Divides the neuron and checks -------------------------------------
% test_divide_neuron(NNET_0, N1_id) -> 
%     ?HEAD("divide_neuron"),
%     NNET_1 = nnet:divide_neuron(NNET_0, N1_id),
%     [N2_id] = network:neurons(NNET_1) -- network:neurons(NNET_0),
%     [N1] = mnesia:read(neuron, N1_id),
%     [N2] = mnesia:read(neuron, N2_id),
%     ?INFO("{Neuron1, Neuron2}: ", {N1,N2}),
%     true = neuron:activation(N1)  == neuron:activation(N2),
%     true = neuron:aggregation(N1) == neuron:aggregation(N2),
%     true = neuron:initializer(N1) == neuron:initializer(N2),
%     true = neuron:bias(N1)        == neuron:bias(N2),
%     LN1 = network:links(N1, NNET_1_id),
%     LN2 = network:links(N2, NNET_1_id),
%     LAll = network:links(NNET_0, N1_id),
%     ?INFO("{Links, {LinksN1, LinksN2}}: ", {LAll, {LN1,LN2}}),
%     [_|_] = LAll -- (LN1 ++ LN2),
%     {ok, NNET_1}.

% % Merges two neurons and checks -------------------------------------
% test_merge_neurons(NNET_0, N1_id, N2_id) -> 
%     ?HEAD("merge_neurons"),
%     % What to do if the 2 neurons had an input???
%     {ok, NNET_1}.




% Connects some neuron and checks -----------------------------------
test_connect_allowed(NNET_0, From, To) -> 
    ?HEAD("connect_allowed"),
    ?INFO("{From,To}: ", {From,To}),
    NNET_1 = nnet:connect_allowed(NNET_0, From, To),
    LAll = [{F,T} || F <- From, T <- To], ?INFO("All: ", LAll),
    LNew = network:links(NNET_1) -- network:links(NNET_0), ?INFO("New: ", LNew),
    Ignored = LAll -- (LAll -- network:links(NNET_0)), ?INFO("Ignored: ", Ignored),
    NotAllowed = get_not_allowed(NNET_0, LAll), ?INFO("Not allowed: ", NotAllowed),
    Null = LAll -- (NotAllowed ++ Ignored ++ LNew),
    ?INFO("[] = LAll -- (NotAllowed ++ Ignored ++ LNew)): ", Null),
    [] = Null,
    {ok, NNET_1}. 

get_not_allowed([L|Lx], NNET_0) -> 
    try network:add_link(NNET_0, L) of
         NNET_1                ->    get_not_allowed(Lx, NNET_1)
    catch error:{bad_link, _} -> [L|get_not_allowed(Lx, NNET_0)]
    end;
get_not_allowed([], _NNET) -> [].

% Disconnects some neuron and checks --------------------------------
test_disconnect_all(NNET_0, From, To) -> 
    ?HEAD("disconnect_all"),
    ?INFO("{From,To}: ", {From,To}),
    NNET_1 = nnet:disconnect_all(NNET_0, From, To),
    LAll = [{F,T} || F <- From, T <- To], ?INFO("All: ", LAll),
    LNNET_1 = network:links(NNET_1),
    Null   = LNNET_1 -- (LNNET_1 -- LAll),
    ?INFO("[] = LNNET_1 -- (LNNET_1 -- LAll): ", Null),
    [] = Null,
    {ok, NNET_1}. 


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% Returns a random neuron from the network --------------------------
random_neuron(NNET) -> 
    ltools:randnth(network:neurons(NNET)).

% Return random neurons from the network ----------------------------
random_neurons(NNET) -> 
    Neurons = network:neurons(NNET),
    case ltools:rand(Neurons, 0.75) of 
        []       -> random_neurons(NNET); % Try again
        Neurons  -> random_neurons(NNET); % Try again
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

