%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%% Created :
%%%-------------------------------------------------------------------
-module(nnet_edit_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(HEAD, [$- || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text), ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$_ || _ <-lists:seq(1,80)]).
-define(END(V), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [V]), V).

-define(INFO(A,B),    ct:log(?LOW_IMPORTANCE,    "~p: ~p",   [A,B])).
-define(ERROR(Error), ct:pal( ?HI_IMPORTANCE, "Error: ~p", [Error])).


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
    ok = application:start(nnet),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok = application:stop(mnesia),
    ok = application:stop(nnet),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Id = nnet:compile(test_architectures:example()),
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
        {sequence_neuron_edits, [sequence, shuffle],
         [
             correct_bias_reinitialisation,
             correct_switch_activation,
             correct_weights_reinitialisation
         ]
        },
        {sequence_network_edits, [sequence, shuffle],
         [
             correct_neuron_division,
             correct_connect_allowed,
             correct_test_disconnect_all
         ]
        },
        {parallel_edits, [parallel, shuffle],
         [correct_bias_reinitialisation    || _ <- lists:seq(1,5)] ++ 
         [correct_switch_activation        || _ <- lists:seq(1,5)] ++
         [correct_weights_reinitialisation || _ <- lists:seq(1,5)] ++     
         [correct_neuron_division          || _ <- lists:seq(1,5)] ++
         [correct_connect_allowed          || _ <- lists:seq(1,5)] ++
         [correct_test_disconnect_all      || _ <- lists:seq(1,5)]
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
        {group, sequence_neuron_edits},
        {group, sequence_network_edits},
        {group, parallel_edits}
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
            N_id = random_neuron(NNET_0),
            Neuron = hd(mnesia:read(neuron, N_id)),
            ok = mnesia:write(neuron:bias(Neuron, 1.0)), %Set
            {ok, NNET_1} = test_reinitialise_bias(NNET_0, N_id),
            NNET_1
        end,
    {atomic, NNET} = nnet_edit:transaction(?config(network_id, Config), Test),
    ?END({ok, NNET}).

% -------------------------------------------------------------------
correct_switch_activation(Config) ->
    ?HEAD("Correct activation switch ............................."),
    Test = 
        fun(NNET_0) -> 
            N_id = random_neuron(NNET_0),
            Neuron = hd(mnesia:read(neuron, N_id)),
            ok = mnesia:write(neuron:activation(Neuron, direct)),
            {ok, NNET_1} = switch_activation(NNET_0, N_id, elu),
            NNET_1
        end,
    {atomic, NNET} = nnet_edit:transaction(?config(network_id, Config), Test),
    ?END({ok, NNET}).

% -------------------------------------------------------------------
correct_weights_reinitialisation(Config) ->
    ?HEAD("Correct weights reinitialisation ......................"),
    Test = 
        fun(NNET_0) -> 
            N_id = random_neuron(NNET_0),
            Links  = network:in_links(NNET_0, N_id),
            [ok = link:write(L, 1.0) || L <- Links], %Set
            {ok, NNET_1} = test_reinitialise_weights(NNET_0, N_id),
            NNET_1
        end,
    {atomic, NNET} = nnet_edit:transaction(?config(network_id, Config), Test),
    ?END({ok, NNET}).

% -------------------------------------------------------------------
correct_neuron_division(Config) ->
    ?HEAD("Correct neuron division ..............................."),
    Test = 
        fun(NNET_0) -> 
            N_id = random_neuron(NNET_0),
            {ok, NNET_1} = test_divide_neuron(NNET_0, N_id),
            NNET_1
        end,
    {atomic, NNET} = nnet_edit:transaction(?config(network_id, Config), Test),
    ?END({ok, NNET}).

% -------------------------------------------------------------------
correct_connect_allowed(Config) -> 
    ?HEAD("Correct connection of multiple neurons ................"),
    Test = 
        fun(NNET_0) -> 
            From_ids = random_neurons(NNET_0),
            To_ids   = random_neurons(NNET_0),
            {ok, NNET_1} = test_connect_allowed(NNET_0, From_ids, To_ids),
            NNET_1
        end,
    {atomic, NNET} = nnet_edit:transaction(?config(network_id, Config), Test),
    ?END({ok, NNET}). 

% -------------------------------------------------------------------
correct_test_disconnect_all(Config) -> 
    ?HEAD("Correct disconnection of multiple neurons ............."),
    Test = 
        fun(NNET_0) -> 
            From_ids = random_neurons(NNET_0),
            To_ids   = random_neurons(NNET_0),
            {ok, NNET_1} = test_disconnect_all(NNET_0, From_ids, To_ids),
            NNET_1
        end,
    {atomic, NNET} = nnet_edit:transaction(?config(network_id, Config), Test),
    ?END({ok, NNET}). 


% --------------------------------------------------------------------
% INDIVIDUAL TEST FUNCTIONS ------------------------------------------

% Reinitialises the bias and checks its value is undefined ----------
test_reinitialise_bias(NNET_0, N_id) -> 
    ?HEAD("reinitialise_bias"),
    NNET_1 = nnet_edit:reinitialise_bias(NNET_0, N_id),
    [Neuron] = mnesia:read(neuron, N_id),
    Bias = neuron:bias(Neuron),
    ?INFO("Bias: ", Bias),
    undefined = Bias,
    {ok, NNET_1}.

% Changes the neuron activation function and checks -----------------
switch_activation(NNET_0, N_id, Func) -> 
    ?HEAD("switch_activation"),
    NNET_1 = nnet_edit:switch_activation(NNET_0, N_id, Func),
    [Neuron] = mnesia:read(neuron, N_id),
    Activation = neuron:activation(Neuron),
    ?INFO("{activation:, expected:} ", {Activation, Func}),
    Func = Activation,
    {ok, NNET_1}.

% Reinitialises the inputs weights and checks -----------------------
test_reinitialise_weights(NNET_0, N_id) -> 
    ?HEAD("reinitialise_weights"),
    NNET_1 = nnet_edit:reinitialise_weights(NNET_0, N_id),
    Weights = [link:read(L) || L <- network:in_links(NNET_0, N_id)],
    ?INFO("Weights: ", Weights),
    [undefined = W || W <- Weights],
    {ok, NNET_1}.

% Divides the neuron and checks -------------------------------------
test_divide_neuron(NNET_0, N1_id) -> 
    ?HEAD("divide_neuron"),
    NNET_1 = nnet_edit:divide_neuron(NNET_0, N1_id),
    [N2_id] = network:neurons(NNET_1) -- network:neurons(NNET_0),
    [N1] = mnesia:read(neuron, N1_id),
    [N2] = mnesia:read(neuron, N2_id),
    ?INFO("{Neuron1, Neuron2}: ", {N1,N2}),
    true = neuron:activation(N1)  == neuron:activation(N2),
    true = neuron:aggregation(N1) == neuron:aggregation(N2),
    true = neuron:initializer(N1) == neuron:initializer(N2),
    true = neuron:bias(N1)        == neuron:bias(N2),
    LN1 = network:links(NNET_1, N1_id),
    LN2 = network:links(NNET_1, N2_id),
    LAll = network:links(NNET_0, N1_id),
    ?INFO("{Links, {LinksN1, LinksN2}}: ", {LAll, {LN1,LN2}}),
    [_|_] = LAll -- (LN1 ++ LN2),
    {ok, NNET_1}.

% % Merges two neurons and checks -------------------------------------
% test_merge_neurons(NNET_0, N1_id, N2_id) -> 
%     ?HEAD("merge_neurons"),
%     % What to do if the 2 neurons had an input???
%     {ok, NNET_1}.

% Connects some neuron and checks -----------------------------------
test_connect_allowed(NNET_0, From, To) -> 
    ?HEAD("connect_allowed"),
    ?INFO("{From,To}: ", {From,To}),
    NNET_1 = nnet_edit:connect_allowed(NNET_0, From, To),
    LAll = [{F,T} || F <- From, T <- To], ?INFO("All: ", LAll),
    LNew = network:links(NNET_1) -- network:links(NNET_0), ?INFO("New: ", LNew),
    Ignored = LAll -- (LAll -- network:links(NNET_0)), ?INFO("Ignored: ", Ignored),
    NotAllowed = get_not_allowed(NNET_0, LAll), ?INFO("Not allowed: ", NotAllowed),
    Null = LAll -- (NotAllowed ++ Ignored ++ LNew),
    ?INFO("[] = LAll -- (NotAllowed ++ Ignored ++ LNew)): ", Null),
    [] = Null,
    {ok, NNET_1}. 

get_not_allowed(NNET_0, [L|Lx]) -> 
    try network:add_link(NNET_0, L) of
         NNET_1                ->    get_not_allowed(NNET_1, Lx)
    catch error:{bad_link, _} -> [L|get_not_allowed(NNET_0, Lx)]
    end;
get_not_allowed(_NNET, []) -> [].


% Disconnects some neuron and checks --------------------------------
test_disconnect_all(NNET_0, From, To) -> 
    ?HEAD("disconnect_all"),
    ?INFO("{From,To}: ", {From,To}),
    NNET_1 = nnet_edit:disconnect_all(NNET_0, From, To),
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
    ltools:rand(network:neurons(NNET), 0.2).












% --------------------------------------------------------------------
% RESULTS CONSOLE PRINT ----------------------------------------------

