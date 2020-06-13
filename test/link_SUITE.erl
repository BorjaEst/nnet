%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%% Created :
%%%-------------------------------------------------------------------
-module(link_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(HEAD, [$↓ || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text), ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$^ || _ <-lists:seq(1,80)]).
-define(END(V), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [V]), V).

-define(INFO(A,B),    ct:log(?LOW_IMPORTANCE,    "~p: ~p",   [A,B])).
-define(ERROR(Error), ct:pal( ?HI_IMPORTANCE, "Error: ~p", [Error])).
-define(TEST(Fun),    mnesia:transaction(Fun)).


%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 2}}].

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
    ok = clean_tables(),
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
        {in_sequence_link_operations, [sequence, shuffle],
         [
             correct_seq_add,
             correct_rcc_add,
             correct_del,
             correct_read,
             correct_write,
             correct_seq,
             correct_rcc,
             correct_in,
             correct_move
         ]
        }
        % {in_parallel_operations, [parallel, shuffle],
        %  [correct_rnode          || _ <- lists:seq(1,5)] ++
        % }
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
        {group, in_sequence_link_operations}
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
correct_seq_add(_Config) ->
    ?HEAD("Correct seq add ......................................."),
    {atomic, Result} = mnesia:transaction(
        fun() -> 
            Links =  [{a,b},{b,c},{a,a}],
            ?INFO("Writing seq links: ", Links),
            [ok = link:add(L, seq, 1.0) || L <- Links],
            [true  = is_in_inTable(L)   || L <- Links], 
            [true  = is_in_seqTable(L)  || L <- Links], 
            [false = is_in_rccTable(L)  || L <- Links], 
            [true  = is_weight(L, 1.0)  || L <- Links], 
            ok
        end
    ),
    ?END(Result).

% -------------------------------------------------------------------
correct_rcc_add(_Config) ->
    ?HEAD("Correct rcc add ........................................"),
    {atomic, Result} = mnesia:transaction(
        fun() -> 
            Links =  [{a,b},{b,c},{a,a}],
            ?INFO("Writing rcc links: ", Links),
            [ok = link:add(L, rcc, 1.0) || L <- Links],
            [true  = is_in_inTable(L)   || L <- Links], 
            [false = is_in_seqTable(L)  || L <- Links], 
            [true  = is_in_rccTable(L)  || L <- Links], 
            [true  = is_weight(L, 1.0)  || L <- Links], 
            ok
        end
    ),
    ?END(Result).

% -------------------------------------------------------------------
correct_del(_Config) ->
    ?HEAD("Correct del ..........................................."),
    {atomic, Result} = mnesia:transaction(
        fun() -> 
            Links =  [{a,b},{b,c},{a,a}],
            ?INFO("Writing seq links: ", Links),
            [ok = link:add(L, seq, 1.0) || L <- Links],
            ?INFO("Deleting links: ", Links),
            [ok = link:del(L)           || L <- Links],
            [false = is_in_inTable(L)   || L <- Links], 
            [false = is_in_seqTable(L)  || L <- Links], 
            [false = is_weight(L, 1.0)  || L <- Links],
            ok
        end
    ),
    ?END(Result).

% -------------------------------------------------------------------
correct_read(_Config) -> 
    ?HEAD("Correct read ..........................................."),
    {atomic, Result} = mnesia:transaction(
        fun() -> 
            Links =  [{a,b},{b,c},{a,a}],
            ?INFO("Writing seq links: ", Links),
            [ok = link:add(L, seq, 1.0) || L <- Links],
            ?INFO("Reading links: ", Links),
            Weights = [{L,link:read(L)} || L <- Links], 
            ?INFO("Readings {Link, Weight}: ", Weights),
            [true = W==1.0  || {_,W} <- Weights], 
            ?INFO("Deleting links: ", Links),
            [ok = link:del(L)           || L <- Links],
            ?INFO("Reading non existing links: ", Links),
            Errors = [{L, (catch link:read(L))} || L <- Links],
            ?INFO("Readings {Link, Exception}: ", Errors),
            [{_,{{not_defined,{link,L}},_}} = W || {L,W} <- Errors], 
            ok
        end
    ),
    ?END(Result).

% -------------------------------------------------------------------
correct_write(_Config) -> 
    ?HEAD("Correct write .........................................."),
    {atomic, Result} = mnesia:transaction(
        fun() -> 
            Links =  [{a,b},{b,c},{a,a}],
            ?INFO("Writing seq links: ", Links),
            [ok = link:add(L, seq, 1.0) || L <- Links],
            ?INFO("Writing links: ", Links),
            [ok = link:write(L, 2.0)  || L <- Links], 
            [true = is_weight(L, 2.0) || L <- Links],
            ?INFO("Deleting links: ", Links),
            [ok = link:del(L)           || L <- Links],
            ?INFO("Writing non existing links: ", Links),
            Errors = [{L, link:write(L, 3.0)} || L <- Links],
            ?INFO("Writting {Link, Result}: ", Errors),
            [{error,{not_defined,{link,L}}} = W || {L,W} <- Errors], 
            ok
        end
    ),
    ?END(Result).

% -------------------------------------------------------------------
correct_seq(_Config) -> 
    ?HEAD("Correct seq ..........................................."),
    {atomic, Result} = mnesia:transaction(
        fun() -> 
            SeqLinks =  [{a,b},{a,t},{a,a}],
            ?INFO("Writing seq links: ", SeqLinks),
            [ok = link:add(L, seq, 1.0) || L <- SeqLinks],
            RccLinks =  [{a,c},{a,w},{a,x}],
            ?INFO("Writing rcc links: ", RccLinks),
            [ok = link:add(L, rcc, 1.0) || L <- RccLinks],
            ?INFO("Recovering seq links from: ", a),
            true = has_the_same(link:seq(a), SeqLinks),
            ?INFO("Deleting seq links: ", SeqLinks),
            [ok = link:del(L)           || L <- SeqLinks],
            ?INFO("Recovering seq links from: ", a),
            false = has_any(link:seq(a), SeqLinks), 
            ok
        end
    ),
    ?END(Result).

% -------------------------------------------------------------------
correct_rcc(_Config) -> 
    ?HEAD("Correct rcc ..........................................."),
    {atomic, Result} = mnesia:transaction(
        fun() -> 
            SeqLinks =  [{a,b},{a,t},{a,a}],
            ?INFO("Writing seq links: ", SeqLinks),
            [ok = link:add(L, seq, 1.0) || L <- SeqLinks],
            RccLinks =  [{a,c},{a,w},{a,x}],
            ?INFO("Writing rcc links: ", RccLinks),
            [ok = link:add(L, rcc, 1.0) || L <- RccLinks],
            ?INFO("Recovering rcc links from: ", a),
            true = has_the_same(link:rcc(a), RccLinks),
            ?INFO("Deleting rcc links: ", RccLinks),
            [ok = link:del(L)           || L <- RccLinks],
            ?INFO("Recovering rcc links from: ", a),
            false = has_any(link:rcc(a), RccLinks), 
            ok
        end
    ),
    ?END(Result).

% -------------------------------------------------------------------
correct_in(_Config) -> 
    ?HEAD("Correct in ............................................"),
    {atomic, Result} = mnesia:transaction(
        fun() -> 
            SeqLinks =  [{b,a},{t,a},{a,a}],
            ?INFO("Writing seq links: ", SeqLinks),
            [ok = link:add(L, seq, 1.0) || L <- SeqLinks],
            RccLinks =  [{c,a},{w,a},{x,a}],
            ?INFO("Writing rcc links: ", RccLinks),
            [ok = link:add(L, rcc, 1.0) || L <- RccLinks],
            ?INFO("Recovering in links from: ", a),
            true = has_the_same(link:in(a), SeqLinks ++ RccLinks),
            ?INFO("Deleting rcc links: ", RccLinks),
            [ok = link:del(L)           || L <- RccLinks], 
            ?INFO("Recovering in links from: ", a),
            true = has_the_same(link:in(a), SeqLinks),
            ?INFO("Deleting seq links: ", SeqLinks),
            [ok = link:del(L)           || L <- SeqLinks], 
            ?INFO("Recovering seq links from: ", a),
            false = has_any(link:seq(a), SeqLinks ++ RccLinks), 
            ok
        end
    ),
    ?END(Result).

% -------------------------------------------------------------------
correct_move(_Config) ->
    ?HEAD("Correct move ..........................................."),
    {atomic, Result} = mnesia:transaction(
        fun() -> 
            Links =  [{a,b},{b,c},{a,a}],
            ?INFO("Writing seq links: ", Links),
            [ok = link:add(L, seq, 1.0) || L <- Links],
            NMap = #{a=>b},
            ?INFO("Moving links: ", NMap),
            [ok = link:move(L,NMap) || L <- Links],
            [false = is_in_inTable(L)  || L <- [{a,b}, {a,a}]],
            [false = is_in_seqTable(L) || L <- [{a,b}, {a,a}]],
            [false = is_in_rccTable(L) || L <- [{a,b}, {a,a}]],
            true = is_weight({b,c}, 1.0), 
            true = is_weight({b,b}, 2.0), 
            ok
        end
    ),
    ?END(Result).


% --------------------------------------------------------------------
% INDIVIDUAL TEST FUNCTIONS ------------------------------------------

% Checks the link is existing on table link_in ----------------------
is_in_inTable({From,To} = Link) -> 
    ?HEAD("Is the link existing on link_in?"),
    ?INFO("Link: ", Link),
    InLinks = mnesia:read({link_in, To}),
    ?INFO("In links: ", InLinks),
    ?END([From] == [X || {_,_,X} <- InLinks, X=:=From]).

% Checks the link is existing on table link_seq ----------------------
is_in_seqTable({From,To} = Link) -> 
    ?HEAD("Is the link existing on link_seq?"),
    ?INFO("Link: ", Link),
    SeqLinks = mnesia:read({link_seq, From}),
    ?INFO("Seq out links: ", SeqLinks),
    ?END([To] == [X || {_,_,X} <- SeqLinks, X=:=To]).

% Checks the link is existing on table link_rcc ----------------------
is_in_rccTable({From,To} = Link) -> 
    ?HEAD("Is the link existing on link_rcc?"),
    ?INFO("Link: ", Link),
    RccLinks = mnesia:read({link_rcc, From}),
    ?INFO("Rcc out links: ", RccLinks),
    ?END([To] == [X || {_,_,X} <- RccLinks, X=:=To]).

% Checks the link is existing on table link_rcc ----------------------
is_in_wTable(Link) -> 
    ?HEAD("Is the link existing on weights?"),
    ?INFO("Link: ", Link),
    case mnesia:read({weight, Link}) of 
        [{_,_,X}] -> W = X;
        []        -> W = not_defined
    end,
    ?END((W==not_init) or (is_number(W))).

% Checks the link value is the expected -----------------------------
is_weight(Link, Expected) -> 
    ?HEAD("Is the link weight correct?"),
    ?INFO("Link: ", Link),
    ?INFO("Expected: ", Expected),
    case mnesia:read({weight, Link}) of 
        [{_,_,X}] -> Weight = X;
        []        -> Weight = not_defined
    end,
    ?INFO("Weight: ", Weight),
    ?END(Expected == Weight).  

% Checks the 2 lists have the same elements -------------------------
has_the_same(List1, List2) -> 
    ?HEAD("Have this 2 lists the same elements?"),
    ?INFO("List1: ", List1),
    ?INFO("List2: ", List2),
    ?END(lists:sort(List1) == lists:sort(List2)).

% Checks if the list have any element -------------------------------
has_any(Elements, List) -> 
    ?HEAD("Is any of the elements in list?"),
    ?INFO("Elements: ", Elements),
    ?INFO("List: ", List),
    ?END(List =/= (List -- Elements)).


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% Cleans the link table deleting all entries ------------------------
clean_tables() -> 
    {atomic, ok} = mnesia:clear_table(link_in ),
    {atomic, ok} = mnesia:clear_table(link_seq),
    {atomic, ok} = mnesia:clear_table(link_rcc),
    {atomic, ok} = mnesia:clear_table(weight  ),
    ok.


% --------------------------------------------------------------------
% RESULTS CONSOLE PRINT ----------------------------------------------

