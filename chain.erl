-module(chain).
-export([start/0, serv1/0, serv2/0, serv3/1]).

start() ->
    Serv1 = spawn(chain, serv1, []),
    Serv2 = spawn(chain, serv2, []),
    Serv3 = spawn(chain, serv3, [0]),
    link(Serv1),
    link(Serv2),
    link(Serv3),
    % Serv1 ! {add, 1, 2}.
    register(serv2, Serv2),
    register(serv3, Serv3),
    loop(Serv1, Serv2, Serv3).

% loop(Serv1, Serv2, Serv3) ->
%     io:format("Enter message (or 'all_done' to quit):~n"),
%     Input = string:trim(io:get_line("")), % Read input and trim leading/trailing whitespace
%     % io:format("Parsed is: ~s.", [Input]),
%     case Input of
%         "all_done" ->
%             serv1 ! halt;
%         _ ->
%             % Try parsing the input safely, handle errors if input is invalid
%             case erl_scan:string(Input) of

%                 {ok, Tokens, _} ->
%                     case erl_parse:parse_exprs(Tokens) of
%                         {ok, [Parsed]} ->
%                             TrimmedInput = string:trim(Input, trailing, "."),
%                             % io:format("Parsed is: ~s.", [Parsed]),
%                             io:format("Parsed is: ~s~n.", [TrimmedInput]),
%                             io:format("Parsed is: ~s~n.", [Input]),
%                             Serv1 ! Input,
%                             Serv1 ! {add, 1, 3},
%                             Serv1 ! Parsed,
%                             % serv1 ! Input,
%                             loop(Serv1, Serv2, Serv3);
%                         _Error ->
%                             io:format("Invalid input. Please try again.~n"),
%                             loop(Serv1, Serv2, Serv3)
%                     end;
%                 _Error ->
%                     io:format("Invalid input. Please try again.~n"),
%                     loop(Serv1, Serv2, Serv3)
%             end
%     end.

loop(Serv1, Serv2, Serv3) ->
    io:format("Enter message (or 'all_done' to quit):~n"),
    Input = string:trim(io:get_line("")),
    case Input of
        "all_done" ->
            Serv1 ! halt;
        _ ->
            % Try parsing the input safely, handle errors if input is invalid
            case catch erl_scan:string(Input ++ ".") of
                {ok, Tokens, _} ->
                    case catch erl_parse:parse_exprs(Tokens) of
                        {ok, [Parsed]} ->
                            % The parsed result might be an AST form, handle accordingly
                            case Parsed of
                                {tuple, _, Elements} ->
                                    % Convert AST representation to an Erlang tuple
                                    Tuple = list_to_tuple([element(3, E) || E <- Elements]),
                                    Serv1 ! Tuple,
                                    loop(Serv1, Serv2, Serv3);
                                _ ->
                                    io:format("Unsupported command. Please try again.~n"),
                                    loop(Serv1, Serv2, Serv3)
                            end;
                        _ ->
                            io:format("Invalid input. Please try again.~n"),
                            loop(Serv1, Serv2, Serv3)
                    end;
                _ ->
                    io:format("Invalid input. Please try again.~n"),
                    loop(Serv1, Serv2, Serv3)
            end
    end.




serv1() ->
    receive
        halt ->
            io:format("(serv1) Halting~n"),
            whereis(serv2) ! halt;
        {add, A, B} when is_number(A), is_number(B) ->
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
            whereis(serv2) ! Other,
            serv1()
    end.

serv2() ->
    receive
        halt ->
            io:format("(serv2) Halting~n"),
            whereis(serv3) ! halt;
        [H | T] when is_integer(H) ->
            Sum = lists:sum([X || X <- [H | T], is_number(X)]),
            io:format("(serv2) Sum of list elements = ~p~n", [Sum]),
            serv2();
        [H | T] when is_float(H) ->
            Product = lists:foldl(fun(X, Acc) -> X * Acc end, 1, [X || X <- [H | T], is_number(X)]),
            io:format("(serv2) Product of list elements = ~p~n", [Product]),
            serv2();
        Other ->
            whereis(serv3) ! Other,
            serv2()
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