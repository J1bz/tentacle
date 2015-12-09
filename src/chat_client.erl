-module(chat_client).
-export([start/2]).
-export([send/1, send/2]).
-export([disconnect/0]).

-define(TCP_OPTIONS, [binary,
                      {packet, 0},
                      {active, false},
                      {reuseaddr, true}]).

start(Address, Port) ->
    spawn(fun() ->
            start_client(Address, Port),
            common:sleep(infinity)
          end).

start_client(Str_Address, Port) ->
    case inet:parse_address(Str_Address) of
        {ok, Parsed_Address} ->
            case gen_tcp:connect(Parsed_Address, Port, ?TCP_OPTIONS, 5000) of
                {ok, Socket} ->
                    io:format("Connection established~n"),
                    register(client_pid, spawn(fun() -> client(Socket) end)),
                    spawn(fun() -> listen_server_notifications(Socket) end);
                {error, Error} ->
                    io:format("Error ~s~n", [Error])
            end;
        {error, Error} ->
            io:format("Error ~s~n", [Error])
    end.

client(Socket) ->
    receive
        {send, Message} ->
            print_send(Message),
            gen_tcp:send(Socket, common:format("Data~n~s~n", [Message])),
            client(Socket);
        {send, Message, To_String} ->
            print_send_to(Message, To_String),
            gen_tcp:send(Socket, common:format("Data~n~s~n~s~n",
                                               [Message, To_String])),
            client(Socket);
        {received, Frame} ->
            handle_received(Frame),
            client(Socket);
        {disconnect} ->
            io:format("Disconnecting...~n"),
            gen_tcp:send(Socket, "Absence");
        stop ->
            true
    end.

print_send(Message) ->
    io:format("Me: ~s~n", [Message]).

print_send_to(Message, To_String) ->
    io:format("Me to ~s: ~s~n", [To_String, Message]).

handle_received(Frame) ->
    case common:parse_frame(Frame) of
        {data, {From_String, Message}} ->
            print_data(From_String, Message);
        {presence, Socket_String} ->
            print_presence(Socket_String);
        {absence, Socket_String} ->
            print_absence(Socket_String)
        %TODO: le cas ou aucun des trois n'est recu
    end.

print_data(From_String, Message) ->
    io:format("~s: ~s~n", [From_String, Message]).

print_presence(Socket_String) ->
    io:format("~s joined server~n", [Socket_String]).

print_absence(Socket_String) ->
    io:format("~s left server~n", [Socket_String]).

send(Message) ->
    client_pid ! {send, Message},
    ok.

send(Message, To_String) ->
    client_pid ! {send, Message, To_String},
    ok.

disconnect() ->
    client_pid ! {disconnect},
    ok.

listen_server_notifications(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Frame} ->
            client_pid ! {received, Frame},
            listen_server_notifications(Socket);
        {error, closed} ->
            io:format("Connection lost~n"),
            ok
    end.
