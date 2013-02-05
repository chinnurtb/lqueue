-module(lqueue).

%% Creation, inspection and conversion
-export([new/1,
         is_lqueue/1,
         is_empty/1,
         is_full/1,
         max_len/1,
         len/1,
         to_list/1,
         from_list/2,
         member/2]).

%% Insert and take
-export([in/2,
         in_r/2,
         out/1,
         out_r/1]).

-export([get/1,
         get_r/1,
         peek/1,
         peek_r/1,
         drop/1,
         drop_r/1]).

%% @TODO reverse, join, split, filter

-include("lqueue.hrl").

-spec new(max_length()) -> lqueue().
new(M) when is_integer(M), M > 0 ->
    {0, M, [], []};
new(M) ->
    erlang:error(badarg, [M]).

-spec is_lqueue(lqueue()) -> boolean().
is_lqueue({L, M, R, F}) when is_integer(L), is_integer(M), L >= 0, M > 0, L =< M,
                             is_list(R), is_list(F) ->
    true;
is_lqueue(_) ->
    false.

-spec is_empty(lqueue()) -> boolean().
is_empty({0, M, R, F}) when is_integer(M), M > 0, is_list(R), is_list(F) ->
    true;
is_empty({L, M, R, F}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                            is_list(R), is_list(F) ->
    false;
is_empty(LQ) ->
    erlang:error(badarg, [LQ]).

-spec is_full(lqueue()) -> boolean().
is_full({M, M, R, F}) when is_integer(M), M > 0, is_list(R), is_list(F) ->
    true;
is_full({L, M, R, F}) when is_integer(L), is_integer(M), L >= 0, M > 0, L =< M,
                           is_list(R), is_list(F) ->
    false;
is_full(LQ) ->
    erlang:error(badarg, [LQ]).

-spec max_len(lqueue()) -> max_length().
max_len({L, M, R, F}) when is_integer(L), is_integer(M), L >= 0, M > 0, L =< M,
                           is_list(R), is_list(F) ->
    M;
max_len(LQ) ->
    erlang:error(badarg, [LQ]).

-spec len(lqueue()) -> length().
len({L, M, R, F}) when is_integer(L), is_integer(M), L >= 0, M > 0, L =< M,
                       is_list(R), is_list(F)->
    L;
len(LQ) ->
    erlang:error(badarg, [LQ]).

-spec to_list(lqueue()) -> list().
to_list({L, M, R, F}) when is_integer(L), is_integer(M), L >= 0, M > 0, L =< M,
                           is_list(R), is_list(F) ->
    F ++ lists:reverse(R, []);
to_list(LQ) ->
    erlang:error(badarg, [LQ]).

-spec from_list(list(), max_length()) -> lqueue().
from_list([], M) when is_integer(M), M > 0 ->
    {0, M, [], []};
from_list(L, M) when is_integer(M), M > 0, is_list(L), M >= length(L) ->
    {length(L), M, lists:reverse(L), []};
from_list(L, M) ->
    erlang:error(badarg, [L, M]).

-spec member(term(), lqueue()) -> boolean().
member(_X, {0, M, [], []}) when is_integer(M), M > 0 ->
    false;
member(X, {L, M, R, F}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                             is_list(R), is_list(F) ->
    lists:member(X, R) orelse lists:member(X, F);
member(X, LQ) ->
    erlang:error(badarg, [X, LQ]).

-spec in(term(), lqueue()) -> lqueue().
in(X, {L, M, R, F}) when is_integer(L), is_integer(M), L >= 0, M > 0, L < M,
                         is_list(R), is_list(F) ->
    {L + 1, M, [X | R], F};
in(X, {M, M, R, [_H | T]}) when is_integer(M), M > 0, is_list(R) ->
    {M, M, [X | R], T};
in(X, {M, M, R, []}) when is_integer(M), M > 0, is_list(R) ->
    in(X, {M, M, [], lists:reverse(R)});
in(X, LQ) ->
    erlang:error(badarg, [X, LQ]).

-spec in_r(term(), lqueue()) -> lqueue().
in_r(X, {L, M, R, F}) when is_integer(L), is_integer(M), L >= 0, M > 0, L < M,
                           is_list(R), is_list(F) ->
    {L + 1, M, R, [X | F]};
in_r(X, {M, M, [_H | T], F}) when is_integer(M), M > 0, is_list(F) ->
    {M, M, T, [X | F]};
in_r(X, {M, M, [], F}) when is_integer(M), M > 0, is_list(F) ->
    in(X, {M, M, lists:reverse(F), []});
in_r(X, LQ) ->
    erlang:error(badarg, [X, LQ]).

-spec out(lqueue()) -> {empty, lqueue()} | {{value, term()}, lqueue()}.
out({0, M, [], []} = LQ) when is_integer(M), M > 0 ->
    {empty, LQ};
out({L, M, R, [H | T]}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                              is_list(R) ->
    {{value, H}, {L - 1, M, R, T}};
out({L, M, R, []}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                        is_list(R) ->
    out({L, M, [], lists:reverse(R)});
out(LQ) ->
    erlang:error(badarg, [LQ]).

-spec out_r(lqueue()) -> {'empty', lqueue()} | {{'value', term()}, lqueue()}.
out_r({0, M, [], []} = LQ) when is_integer(M), M > 0 ->
    {empty, LQ};
out_r({L, M, [H | T], F}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                               is_list(F) ->
    {{value, H}, {L - 1, M, T, F}};
out_r({L, M, [], F}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                          is_list(F) ->
    out_r({L, M, lists:reverse(F), []});
out_r(LQ) ->
    erlang:error(badarg, [LQ]).

-spec get(lqueue()) -> term().
get({0, M, [], []} = LQ) when is_integer(M), M > 0 ->
    erlang:error(empty, [LQ]);
get({L, M, R, F}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                       is_list(R), is_list(F) ->
    get(R, F);
get(LQ) ->
    erlang:error(badarg, [LQ]).

-spec get_r(lqueue()) -> term().
get_r({0, M, [], []} = LQ) when is_integer(M), M > 0 ->
    erlang:error(empty, [LQ]);
get_r({L, M, R, F}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                         is_list(R), is_list(F) ->
    get_r(R, F);
get_r(LQ) ->
    erlang:error(badarg, [LQ]).

-spec peek(lqueue()) -> 'empty' | {'value', term()}.
peek({0, M, [], []}) when is_integer(M), M > 0 ->
    empty;
peek({L, M, R, F}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                        is_list(R), is_list(F) ->
    {value, get(R, F)};
peek(LQ) ->
    erlang:error(badarg, [LQ]).

-spec peek_r(lqueue()) -> 'empty' | {'value', term()}.
peek_r({0, M, [], []}) when is_integer(M), M > 0 ->
    empty;
peek_r({L, M, R, F}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                          is_list(R), is_list(F) ->
    {value, get_r(R, F)};
peek_r(LQ) ->
    erlang:error(badarg, [LQ]).

-spec drop(lqueue()) -> lqueue().
drop({0, M, [], []} = LQ) when is_integer(M), M > 0 ->
    erlang:error(empty, [LQ]);
drop({L, M, R, [_H | T]}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                        is_list(R) ->
    {L - 1, M, R, T};
drop({L, M, R, []}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                         is_list(R) ->
    drop({L, M, [], lists:reverse(R)});
drop(LQ) ->
    erlang:error(badarg, [LQ]).

-spec drop_r(lqueue()) -> lqueue().
drop_r({0, M, [], []} = LQ) when is_integer(M), M > 0 ->
    erlang:error(empty, [LQ]);
drop_r({L, M, [_H | T], F}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                                 is_list(F) ->
    {L - 1, M, T, F};
drop_r({L, M, [], F}) when is_integer(L), is_integer(M), L > 0, M > 0, L =< M,
                           is_list(F) ->
    drop_r({L, M, lists:reverse(F), []});
drop_r(LQ) ->
    erlang:error(badarg, [LQ]).

%% Internal functions

get(_, [FH | _]) ->
    FH;
get([RH], []) ->
    RH;
get([_ | RT], []) ->
    lists:last(RT).

get_r([RH | _], _) ->
    RH;
get_r([], [FH]) ->
    FH;
get_r([], [_ | FT]) ->
    lists:last(FT).
