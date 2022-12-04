% --- Day 4: Camp Cleanup ---
-module(main).
-export([read_integers/1, read_integers/2]).
-export([fully_contain/1, fully_contain/2, ranges_overlap/1, ranges_overlap/2]).
-export([start/1]).

start([Filename]) ->
    {ok, Device} = file:open(Filename, [read]),
    X = read_integers(Device),
    io:fwrite("~nOne of assignments fully contains the other: ~w~n", [fully_contain(X)]),
    io:fwrite("Assignment pairs have the ranges overlap: ~w~n", [ranges_overlap(X)]).

read_integers(Device) ->
    read_integers(Device, []).
read_integers(Device, Acc) ->
    case io:fread(Device, [], "~d-~d,~d-~d") of
    eof ->
        lists:reverse(Acc);
    {ok, [D1, D2, D3, D4]} ->
        read_integers(Device, [{D1, D2, D3, D4} | Acc]);
    {error, What} ->
        io:format("io:fread error: ~w~n", [What]),
        read_integers(Device, Acc)
    end.

fully_contain(L) -> fully_contain(L, 0).
fully_contain([], Acc) -> Acc;
fully_contain([{S1, E1, S2, E2} | T], Acc) ->
    if
        (S1 =< S2) and (E1 >= E2) ->
            fully_contain(T, Acc + 1);
        (S2 =< S1) and (E2 >= E1) ->
            fully_contain(T, Acc + 1);
        true ->
            fully_contain(T, Acc)
    end.

ranges_overlap(L) -> ranges_overlap(L, 0).
ranges_overlap([], Acc) -> Acc;
ranges_overlap([{S1, E1, S2, E2} | T], Acc) ->
    if
        (S1 >= S2) and (S1 =< E2) ->
            ranges_overlap(T, Acc + 1);
        (S2 >= S1) and (S2 =< E1) ->
            ranges_overlap(T, Acc + 1);
        true ->
            ranges_overlap(T, Acc)
    end.
