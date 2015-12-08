-module(chat_client).
-export([start/2, send/1, send/2, disconnect/0]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Address, Port) ->
    spawn(fun() ->
            start_client(Address, Port),
            common:sleep(infinity)
          end).

start_client(Address, Port) ->
    case inet:parse_address(Address) of
        {ok, Parsed_Address} ->
            case gen_tcp:connect(Parsed_Address, Port, ?TCP_OPTIONS, 10) of
                {ok, Socket} ->
                    io:format("Connection established~n"),
                    register(client_pid, spawn(fun() -> client(Socket) end)),
                    spawn(fun() -> listen_server_notifications(Socket) end);
                {error, Message} ->
                    io:format("Error ~s~n", [Message])
            end;
        {error, Message} ->
            io:format("Error ~s~n", [Message])
    end.

client(Socket) ->
    receive
        {send, Data} ->
            %io:format("Sending ~s~n", [Data]),
            print_send(Data),
            gen_tcp:send(Socket, common:format("Data~n~s~n", [Data])),
            client(Socket);
        {send, Data, To} ->
            %io:format("Sending ~p to ~p~n", [Data, To]),
            print_send_to(Data, To),
            gen_tcp:send(Socket, common:format("Data~n~s~n~s~n", [Data, To])),
            client(Socket);
        {received, Data} ->
            %io:format("Received: ~s~n", [Data]),
            handle_received(Data),
            client(Socket);
        {disconnect} ->
            io:format("Disconnecting...~n"),
            gen_tcp:send(Socket, "Absence");
        stop ->
            true
    end.

print_send(Data) ->
    io:format("Me: ~s~n", [Data]).

print_send_to(Data, To) ->
    io:format("Me to ~s: ~s~n", [To, Data]).

handle_received(Data) ->
    Splitted_Data = string:tokens(binary_to_list(Data), "\n"),
    [Header | Message] = Splitted_Data,
    case Header of
        "Data" ->
            print_data(Message);
        "Presence" ->
            print_presence([Message]);
        "Absence" ->
            print_absence([Message])
    end.

print_data(Message) ->
    [Content | [From]] = Message,
    io:format("~s: ~s~n", [From, Content]).

print_presence(Socket_Name) ->
    io:format("~s joined server~n", [Socket_Name]).

print_absence(Socket_Name) ->
    io:format("~s left server~n", [Socket_Name]).

send(Data) ->
    client_pid ! {send, Data},
    ok.

send(Data, To) ->
    client_pid ! {send, Data, To},
    ok.

disconnect() ->
    client_pid ! {disconnect},
    ok.

listen_server_notifications(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            client_pid ! {received, Data},
            listen_server_notifications(Socket);
        {error, closed} ->
            io:format("Connection lost~n"),
            ok
    end.
