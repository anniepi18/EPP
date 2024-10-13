-module(chain).
-export([start/0, serv1/0, serv2/0, serv3/1, serv1_hot/0, serv2_hot/0, serv3_hot/1, is_all_integers/1, is_all_floats/1, is_likely_string/1]).

start() ->
    Serv1 = spawn(chain, serv1, []),
    Serv2 = spawn(chain, serv2, []),
    Serv3 = spawn(chain, serv3, [0]),
    link(Serv1),
    link(Serv2),
    link(Serv3),
    register(serv2, Serv2),
    register(serv3, Serv3),
    loop(Serv1, Serv2, Serv3).

startHot1(Serv2, Serv3) ->
    Serv1_hot = spawn(chain, serv1_hot, []),
    unlink(Serv2),
    unlink(Serv3),
    unregister(serv2),
    unregister(serv3),

    link(Serv1_hot),
    link(Serv2),
    link(Serv3),
    register(serv2, Serv2),
    register(serv3, Serv3),
    loop(Serv1_hot, Serv2, Serv3).

startHot2(Serv1, Serv3) ->
    Serv2_hot = spawn(chain, serv2_hot, []),

    % Unlink existing processes
    unlink(Serv1),
    unlink(Serv3),

    % Unregister only registered processes
    unregister(serv2),
    unregister(serv3),

    % Link the new processes
    link(Serv1),
    link(Serv2_hot),
    link(Serv3),

    % Register the new hot swapped version of serv2
    register(serv2, Serv2_hot),
    register(serv3, Serv3),

    % Continue the loop with updated processes
    loop(Serv1, Serv2_hot, Serv3).


startHot3(Serv1, Serv2) ->
    Serv3_hot = spawn(chain, serv3_hot, [0]),

    % Unlink existing processes
    unlink(Serv1),
    unlink(Serv2),

    % Unregister only the registered names
    unregister(serv2),
    unregister(serv3),

    % Link the new processes
    link(Serv1),
    link(Serv2),
    link(Serv3_hot),

    % Register the new hot-swapped version of serv3
    register(serv2, Serv2),
    register(serv3, Serv3_hot),

    % Continue the loop with updated processes
    loop(Serv1, Serv2, Serv3_hot).



loop(Serv1, Serv2, Serv3) ->
    io:format("Enter message (or 'all_done' to quit):~n"),
    {ok, Input} = io:read(""),
    case Input of
        "all_done" ->
            Serv1 ! halt;
        "update1" ->
            io:format("Hot swapping to new version of server 1~n"),
            startHot1(Serv2, Serv3);
        "update2" ->
            io:format("Hot swapping to new version of server 2~n"),
            startHot2(Serv1, Serv3);
        "update3" ->
            io:format("Hot swapping to new version of server 3~n"),
            startHot3(Serv1, Serv2);
        _ ->
            Serv1 ! Input,
            loop(Serv1, Serv2, Serv3)   
    end.

serv1() ->
    receive
        halt ->
            io:format("(serv1) Halting~n"),
            whereis(serv2) ! halt;  % Send halt to serv2
        {'add', A, B} when is_number(A), is_number(B) ->
            Result = A + B,
            io:format("(serv1) ~p + ~p = ~p~n", [A, B, Result]),
            serv1();
        {sub, A, B} when is_number(A), is_number(B) ->
            Result = A - B,
            io:format("(serv1) ~p - ~p = ~p~n", [A, B, Result]),
            serv1();
        {mult, A, B} when is_number(A), is_number(B) ->
            Result = A * B,
            io:format("(serv1) ~p * ~p = ~p~n", [A, B, Result]),
            serv1();
        {divv, A, B} when is_number(A), is_number(B), B =/= 0 ->
            Result = A / B,
            io:format("(serv1) ~p / ~p = ~p~n", [A, B, Result]),
            serv1();
        {neg, A} when is_number(A) ->
            Result = -A,
            io:format("(serv1) neg ~p = ~p~n", [A, Result]),
            serv1();
        {sqrt, A} when is_number(A), A >= 0 ->
            Result = math:sqrt(A),
            io:format("(serv1) sqrt(~p) = ~p~n", [A, Result]),
            serv1();
        Other ->
            io:format("(serv1) Forwarding unrecognized input: ~p to serv2~n", [Other]),
            whereis(serv2) ! Other,
            serv1()
    end.


serv1_hot() ->
    receive
        halt ->
            io:format("(serv1_hot) Halting~n"),
            whereis(serv2) ! halt;  % Send halt to serv2
        {'add', A, B} when is_number(A), is_number(B) ->
            Result = A + B,
            io:format("(serv1_hot) ~p + ~p = ~p~n", [A, B, Result]),
            serv1_hot();
        {sub, A, B} when is_number(A), is_number(B) ->
            Result = A - B,
            io:format("(serv1_hot) ~p - ~p = ~p~n", [A, B, Result]),
            serv1_hot();
        {mult, A, B} when is_number(A), is_number(B) ->
            Result = A * B,
            io:format("(serv1_hot) ~p * ~p = ~p~n", [A, B, Result]),
            serv1_hot();
        {divv, A, B} when is_number(A), is_number(B), B =/= 0 ->
            Result = A / B,
            io:format("(serv1_hot) ~p / ~p = ~p~n", [A, B, Result]),
            serv1_hot();
        {neg, A} when is_number(A) ->
            Result = -A,
            io:format("(serv1_hot) neg ~p = ~p~n", [A, Result]),
            serv1_hot();
        {sqrt, A} when is_number(A), A >= 0 ->
            Result = math:sqrt(A),
            io:format("(serv1_hot) sqrt(~p) = ~p~n", [A, Result]),
            serv1_hot();
        Other ->
            io:format("(serv1_hot) Forwarding unrecognized input: ~p to serv2~n", [Other]),
            whereis(serv2) ! Other,
            serv1_hot()
    end.

serv2() ->
    receive
        halt ->
            io:format("(serv2) Halting~n"),
            whereis(serv3) ! halt;

    % Handle lists in the function body using case statements
        Input when is_list(Input) ->
            case is_all_integers(Input) of
                true ->
                    Sum = lists:sum(Input),
                    io:format("(serv2) Sum of list elements = ~p~n", [Sum]),
                    serv2();
                false ->
                    case is_all_floats(Input) of
                        true ->
                            Product = lists:foldl(fun(X, Acc) -> X * Acc end, 1, Input),
                            io:format("(serv2) Product of list elements = ~p~n", [Product]),
                            serv2();
                        false ->
                            io:format("(serv2) Forwarding unrecognized input: ~p to serv3~n", [Input]),
                            whereis(serv3) ! Input,
                            serv2()
                    end
            end;

    % Any other message is forwarded to serv3
        Other ->
            io:format("(serv2) Forwarding unrecognized input: ~p to serv3~n", [Other]),
            whereis(serv3) ! Other,
            serv2()
    end.

% Helper function to check if all elements in a list are integers
is_all_integers(List) ->
    lists:all(fun is_integer/1, List).

% Helper function to check if all elements in a list are floats
is_all_floats(List) ->
    lists:all(fun is_float/1, List).

is_likely_string(List) ->
    lists:all(fun(X) -> is_integer(X) andalso X >= 32 andalso X =< 126 end, List).

serv2_hot() ->
    receive
        halt ->
            io:format("(serv2_hot) Halting~n"),
            whereis(serv3) ! halt;

    % Handle a list of integers (ensure all elements are integers)
        Input when is_list(Input) ->
            case is_all_integers(Input) of
                true ->
                    Sum = lists:sum(Input),
                    io:format("(serv2_hot) Sum of list elements = ~p~n", [Sum]),
                    serv2_hot();
                false ->
                    case is_all_floats(Input) of
                        true ->
                            Product = lists:foldl(fun(X, Acc) -> X * Acc end, 1, Input),
                            io:format("(serv2_hot) Product of list elements = ~p~n", [Product]),
                            serv2_hot();
                        false ->
                            % Additional check to handle strings explicitly
                            case is_likely_string(Input) of
                                true ->
                                    io:format("(serv2_hot) Forwarding unrecognized string input: ~p to serv3~n", [Input]),
                                    whereis(serv3) ! Input,
                                    serv2_hot();
                                false ->
                                    io:format("(serv2_hot) Forwarding unrecognized input: ~p to serv3~n", [Input]),
                                    whereis(serv3) ! Input,
                                    serv2_hot()
                            end
                    end
            end;

    % Any other message is forwarded to serv3
        Other ->
            io:format("(serv2_hot) Forwarding unrecognized input: ~p to serv3~n", [Other]),
            whereis(serv3) ! Other,
            serv2_hot()
    end.


serv3(Count) ->
    receive
        halt ->
            io:format("(serv3) Halting. Unhandled message count: ~p~n", [Count]);
        {error, Message} ->
            io:format("(serv3) Error: ~p~n", [Message]),
            serv3(Count);
        Other ->
            io:format("(serv3) Not handled: ~p~n", [Other]),
            serv3(Count + 1)
    end.

serv3_hot(Count) ->
    receive
        halt ->
            io:format("(serv3_hot) Halting. Unhandled message count: ~p~n", [Count]);
        {error, Message} ->
            io:format("(serv3_hot) Error: ~p~n", [Message]),
            serv3_hot(Count);
        Other ->
            io:format("(serv3_hot) Not handled: ~p~n", [Other]),
            serv3_hot(Count + 1)
    end.