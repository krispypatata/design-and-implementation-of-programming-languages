% DESCRIPTION: 
%       This is a simple Erlang program that allows two users to communicate with each 
% other (just like a chat application). One user will start the communication process 
% by invoking the init_chat function. The other user connects to the first user by
% invoking the init_chat2 function while specifying the first user's name (passing it as a parameter).
% The communication process terminates when one of the users sends the string 'bye'.
% AUTHOR:   Keith Ginoel S. Gabinete
% CREATED:  October 16, 2024
% ═════════════════════════════════════════════════════════════════════════════════════

-module(gabinete).
-compile([nowarn_export_all,export_all]).

% GLOBAL VARIABLE
-define(MODULE_NAME, gabinete).

% ═════════════════════════════════════════════════════════════════════════════════════
% Initialize the chat/communcation process (setting up the server and server prompt)
init_chat() ->
	% Check if server_listener is already registerd (prevent errors when calling the init_chat function)
	% Unregister server_listener if it is registered
	case whereis(server_listener) of
        undefined -> ok; 			% Do nothing
        _ -> unregister(server_listener)
    end,

	% Ask for the name of the user
    Server_Name = string:trim(io:get_line("Enter Your Name: ")),

	% Wait for another user to connect
	register(chat_connection, spawn(?MODULE_NAME, wait_for_connection, [Server_Name])).
    
% ─────────────────────────────────────────────────────────────────────────────────────
% A function for establishing connection with another user and also for spawning a 
% function to get inputs from the user (server)
wait_for_connection(Server_Name) ->
	receive
		{connection_established, Client_Listener_PID} ->
			io:format("────────────────────────────────~n"), 
			io:format("Connection has been established.~n"),
			io:format("────────────────────────────────~n"),
			register(server_listener, spawn(?MODULE_NAME, handle_server_listening, [Server_Name])),
			handle_server_input(Server_Name, Client_Listener_PID)
			% alternative: spawn handle_server_input instead of calling it
			% spawn(?MODULE_NAME, handle_server_input, [Server_Name, Client_Listener_PID]),
	end.

% ─────────────────────────────────────────────────────────────────────────────────────
% A loop/recursive function for monitoring mails for the server process
handle_server_listening(Server_Name) ->
    receive
        bye ->
            io:format("Your partner has disconnected!~n"),
			erlang:halt(); % Force exit the erlang shell

		% Output message from the client
        {client_message, Client_Name, Client_Input} ->
            io:format("~s: ~s", [Client_Name, Client_Input]),
            handle_server_listening(Server_Name)
    end.

% ─────────────────────────────────────────────────────────────────────────────────────
% Prompt for server inputs
handle_server_input(Server_Name, Client_Listener_PID) ->
	% Ask for user input
	Chat_Tag = io_lib:format("~s: ", [Server_Name]),
	Server_Input = io:get_line(Chat_Tag),

	case string:lowercase(Server_Input) of
		% Exit if "bye"
		"bye\n" ->
			io:format("You have disconnected!~n"),
			Client_Listener_PID ! bye,
			erlang:halt();
		_ ->
			% Send the message to the client
			Client_Listener_PID ! {server_message, Server_Name, Server_Input},
			handle_server_input(Server_Name, Client_Listener_PID)
	end.


% ═════════════════════════════════════════════════════════════════════════════════════
% Initialize the client processs
init_chat2(Chat_Node) ->
	% Check first if the client can ping the server
	case net_adm:ping(Chat_Node) of
		% Success
		pong -> 
			% Check first if Server is already initialized
			case rpc:call(Chat_Node, erlang, whereis, [chat_connection]) of
        		undefined ->
					io:format("────────────────────────────────~n"), 
					io:format("Cannot establish a connection.~n"),
					io:format("init_chat() is not yet invoked.~n"),
					io:format("────────────────────────────────~n"),
					exit(normal);
				_ -> 
					Client_Name = string:trim(io:get_line("Enter Your Name: ")),
					Client_Listener_PID = spawn(?MODULE_NAME, handle_client_listening, []),
					{chat_connection, Chat_Node} ! {connection_established, Client_Listener_PID},
					io:format("────────────────────────────────~n"), 
					io:format("Connection has been established.~n"),
					io:format("────────────────────────────────~n"),
					handle_client_input(Client_Name, Chat_Node)
			end;
		% Failure
		pang -> 
			io:format("──────────────────────────────────────────────────────────~n"), 
			io:format("Cannot connect to ~s. ~n", [Chat_Node]),
			io:format("Please make sure that the server is running and try again.~n"),
			io:format("──────────────────────────────────────────────────────────~n"), 
            erlang:halt() % Force exit the erlang shell
	end.

% ─────────────────────────────────────────────────────────────────────────────────────
% A loop/recursive function for monitoring mails for the client process
handle_client_listening() -> 
	receive
		bye ->
            io:format("Your partner has disconnected!~n"),
			erlang:halt(); % Force exit the erlang shell

		% Output message from the server 
		{server_message, Server_Name, Server_Input} ->
			io:format("~s: ~s", [Server_Name, Server_Input]),
			handle_client_listening()
	end.

% ─────────────────────────────────────────────────────────────────────────────────────
% Prompt for client inputs
handle_client_input(Client_Name, Chat_Node) ->
	% Ask for user input
	Chat_Tag = io_lib:format("~s: ", [Client_Name]),
    Client_Input = io:get_line(Chat_Tag),

    case string:lowercase(Client_Input) of
		% Exit if "bye"
        "bye\n" ->
			io:format("You have disconnected!~n"),
			{server_listener, Chat_Node} ! bye,
			erlang:halt(); % Force exit the erlang shell
        _ ->
			% Send the message to the server
			{server_listener, Chat_Node} ! {client_message, Client_Name, Client_Input},
            handle_client_input(Client_Name, Chat_Node)
    end.
% ═════════════════════════════════════════════════════════════════════════════════════