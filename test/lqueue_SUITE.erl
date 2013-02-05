-module(lqueue_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0,
         groups/0,
         init_per_test_suite/1,
         end_per_test_suite/1,
         init_per_group/2,
         end_per_group/2]).

%% Tests.
-export([wrong_max_size/1,
         inspection/1,
         conversion/1,
         in_out/1,
         in_out2/1]).

%% ct.
all() ->
    [wrong_max_size,
     inspection,
     conversion,
     in_out,
     in_out2].

groups() ->
    [].

init_per_test_suite(Config) ->
    Config.

end_per_test_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% tests
wrong_max_size(_Config) ->
    Res1 = try lqueue:new(0) of
               _ ->
                   ok
           catch
               error:E1 ->
                   E1
           end,
    Res2 = try lqueue:new(-1) of
               _ ->
                   ok
           catch
               error:E2 ->
                   E2
           end,
    badarg = Res1 = Res2,
    ok.

inspection(_Config) ->
    %% empty lqueue, max == 1
    LQ0 = lqueue:new(1),
    true = lqueue:is_empty(LQ0),
    false = lqueue:is_full(LQ0),
    1 = lqueue:max_len(LQ0),
    0 = lqueue:len(LQ0),
    false = lqueue:member(0, LQ0),
    %% 1 element, max == 1
    LQ1 = lqueue:in(atom, LQ0),
    false = lqueue:is_empty(LQ1),
    true = lqueue:is_full(LQ1),
    1 = lqueue:max_len(LQ1),
    1 = lqueue:len(LQ1),
    true = lqueue:member(atom, LQ1),
    %% 1 element, max == 1
    LQ2 = lqueue:in(-1, LQ1),
    false = lqueue:is_empty(LQ2),
    true = lqueue:is_full(LQ2),
    1 = lqueue:max_len(LQ2),
    1 = lqueue:len(LQ2),
    true = lqueue:member(-1, LQ2),
    %% 1 element, max == 2
    LQ3 = lqueue:new(2),
    LQ4 = lqueue:in("abc", LQ3),
    false = lqueue:is_empty(LQ4),
    false = lqueue:is_full(LQ4),
    2 = lqueue:max_len(LQ4),
    1 = lqueue:len(LQ4),
    true = lqueue:member("abc", LQ4),
    false = lqueue:member(1000, LQ4),
    ok.

conversion(_Config) ->
    LQ0 = lqueue:new(1),
    [] = lqueue:to_list(LQ0),
    LQ1 = lqueue:in(5, LQ0),
    [5] = lqueue:to_list(LQ1),
    LQ2 = lqueue:in(10, LQ1),
    [10] = lqueue:to_list(LQ2),
    LQ3 = lqueue:from_list([1,2,3], 3),
    [1, 2, 3] = lqueue:to_list(lqueue:from_list(lqueue:to_list(LQ3), 3)),
    LQ4 = lqueue:from_list([["abc"], ["def"]], 10),
    [["abc"], ["def"]] = lqueue:to_list(LQ4),
    ok.

in_out(_Config) ->
    LQ0 = lqueue:new(3),
    LQ1 = lqueue:in(10, LQ0),
    LQ2 = lqueue:in(20, LQ1),
    LQ3 = lqueue:in(30, LQ2),
    [10, 20, 30] = lqueue:to_list(LQ3),
    LQ4 = lqueue:in(40, LQ3),
    [20, 30, 40] = lqueue:to_list(LQ4),
    LQ5 = lqueue:in(50, LQ4),
    [30, 40, 50] = lqueue:to_list(LQ5),
    LQ6 = lqueue:in_r(60, LQ5),
    [60, 30, 40] = lqueue:to_list(LQ6),
    LQ7 = lqueue:in_r(70, LQ6),
    [70, 60, 30] = lqueue:to_list(LQ7),
    {{value, 70}, LQ8} = lqueue:out(LQ7),
    {{value, 60}, LQ9} = lqueue:out(LQ8),
    {{value, 30}, LQ10} = lqueue:out(LQ9),
    {empty, _} = lqueue:out(LQ10),
    {empty, _} = lqueue:out_r(LQ10),
    LQ11 = lqueue:in(80, LQ10),
    LQ12 = lqueue:in(90, LQ11),
    {{value, 90}, LQ13} = lqueue:out_r(LQ12),
    {{value, 80}, _} = lqueue:out_r(LQ13),
    ok.

in_out2(_Config) ->
    LQ0 = lqueue:new(2),
    LQ1 = lqueue:in(a1, LQ0),
    LQ2 = lqueue:in(a2, LQ1),
    a1 = lqueue:get(LQ2),
    a2 = lqueue:get_r(LQ2),
    {value, a1} = lqueue:peek(LQ2),
    {value, a2} = lqueue:peek_r(LQ2),
    LQ3 = lqueue:drop_r(LQ2),
    1 = lqueue:len(LQ3),
    a1 = lqueue:get_r(LQ3),
    LQ4 = lqueue:drop(LQ3),
    0 = lqueue:len(LQ4),
    empty = lqueue:peek(LQ4),
    Res1 = try lqueue:drop(LQ4) of
               _ ->
                   no_error
           catch
               error:E1 ->
                   E1
           end,
    Res2 = try lqueue:drop_r(LQ4) of
               _ ->
                   no_error
           catch
               error:E2 ->
                   E2
           end,
    empty = Res1 = Res2,
    ok.
