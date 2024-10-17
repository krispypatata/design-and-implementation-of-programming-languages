% DESCRIPTION: 
%       This is a simple Erlang program designed to mimic a live chat application.
% AUTHOR:   Keith Ginoel S. Gabinete
% CREATED:  October 16, 2024
% ═════════════════════════════════════════════════════════════════════════════════════

-module(chat).
-compile([nowarn_export_all,export_all]).

% ═════════════════════════════════════════════════════════════════════════════════════
% Initialize the chat, setting up the server and server prompt
init_chat() ->
	% Check if chat_server is already registerd (prevent errors when calling the init_chat function)
	% Unregister chat_server if it is registered
	case whereis(chat_server) of
        undefined -> ok; 			% Do nothing
        _ -> unregister(chat_server)
    end,

	% Ask for the name of the user
    Server_Name = string:trim(io:get_line("Enter Your Name: ")),
    register(chat_server, spawn(chat, server, [Server_Name])),
    server_prompt(Server_Name).

% Server loop, handling incoming pings and communication
server(Server_Name) ->
    receive
        finished ->
            io:format("Server finished ~n");
        {client_message, Client_Pid, Client_Name, Client_Input} ->
            io:format("~s: ~s", [Client_Name, Client_Input]),
            Client_Pid ! pong,
            server(Server_Name)
    end.

% Prompt for server input
server_prompt(Server_Name) ->
    % io:format("~s: ", [Server_Name]),
    Server_Input = io:get_line("You: "),
    case Server_Input of
        "bye\n" ->
            io:format("bye~n");
        _ ->
            server_prompt(Server_Name)
    end.

% Initialize the client
init_chat2(Chat_Node) ->
    Client_Name = string:trim(io:get_line("Enter Your Name: ")),
    io:format("Spawning client process on node ~p~n", [Chat_Node]),
    client(Client_Name, Chat_Node).

% Client loop, handling input and sending pings
client(Client_Name, Chat_Node) ->
    % io:format("~n=============================================~n"),
    % io:format("Client function called on node ~p~n", [Chat_Node]),  % For debugging
    % io:format("Type 'bye' to exit! ~n"),

    % io:format("~s: ", [Client_Name]),
    Client_Input = io:get_line("You: "),
    % io:format("Client received input: ~s", [Client_Input]),  % For debugging

    case Client_Input of
        "bye\n" ->
            io:format("Client has been disconnected~n");

        % "ping\n" -> 
        %     {chat_server, Chat_Node} ! {ping, self(), Client_Name},
        %     io:format("Sent Ping to server~n"),
        %     client(Client_Name, Chat_Node);
        _ ->
			{chat_server, Chat_Node} ! {client_message, self(), Client_Name, Client_Input},
			% io:format("~s", [Client_Input]),
            client(Client_Name, Chat_Node)
    end.
