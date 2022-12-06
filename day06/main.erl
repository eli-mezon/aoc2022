% --- Day 6: Tuning Trouble ---
-module(main).
-export([get_all_lines/1]).
-export([find_marker/2, find_marker/4]).
-export([start/1]).

start([Filename]) ->
    {ok, Device} = file:open(Filename, [read]),
    Buffer = get_all_lines(Device),
    io:fwrite("Start-of-packet marker: ~w~n", [find_marker(Buffer, 4)]),
    io:fwrite("Start-of-message marker: ~w~n", [find_marker(Buffer, 14)]).

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.

find_marker(Datastream, N) -> find_marker([], Datastream, N, 0).
find_marker(_, _, 0, Pos) -> Pos;
find_marker(Marker, [DHead|DTail], Acc, Pos) ->
    case lists:member(DHead, Marker) of 
        false ->
            find_marker(Marker ++ [DHead], DTail, Acc - 1, Pos + 1);
        true ->
            [_|MTail] = Marker,
            find_marker(MTail, [DHead|DTail], Acc + 1, Pos)
    end.
