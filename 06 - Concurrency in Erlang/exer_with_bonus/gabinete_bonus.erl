% DESCRIPTION: 
%       This is a simple Erlang program that allows three or more users to communicate 
% with each other, similar to a chat application, through a server. One user (the server) 
% initiates the communication process by invoking the init_chat function. Other users 
% can connect to the server by invoking the init_chat2 function and specifying the 
% server's name as a parameter. The communication process terminates when the server sends
% the string 'bye'. Additionally, any connected user can disconnect by sending the string
% 'bye' as well.
% AUTHOR:   Keith Ginoel S. Gabinete
% CREATED:  October 18, 2024
% ═════════════════════════════════════════════════════════════════════════════════════

-module(gabinete_bonus).
-compile([nowarn_export_all,export_all]).

% GLOBAL VARIABLE
-define(MODULE_NAME, gabinete_bonus).

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
    Server_Name = "[S] " ++ string:trim(io:get_line("Enter Your Name: ")),

	% Instantiate a listener for the server
	register(server_listener, spawn(?MODULE_NAME, handle_server_listening, [self(), Server_Name, [{Server_Name, self()}]])),
	
	% For handling user inputs from the user (server)
	handle_server_input(Server_Name, []).

% ─────────────────────────────────────────────────────────────────────────────────────
% A loop/recursive function for monitoring mails for the server process
handle_server_listening(Server_Input_Listener_PID, Server_Name, Client_List) ->
    receive
		% When a client connects to the server
		{connection_established, Client_Listener_PID, Client_Name} ->
			io:format("~s has joined the chat.~n", [Client_Name]),
			broadcast_message(Client_Name, user_connected, Client_List),

			% Update the list of clients
			New_Client_List = [{Client_Name, Client_Listener_PID} | Client_List],
			Server_Input_Listener_PID ! {update_client_list, New_Client_List},
			handle_server_listening(Server_Input_Listener_PID, Server_Name, New_Client_List);

		% When a client disconnects
        {bye, Client_Name, Client_Listener_PID} ->
			io:format("~s has left the chat.~n", [Client_Name]),
			broadcast_message(Client_Name, user_disconnected, Client_List),

			% Update the list of clients
			New_Client_List = lists:delete({Client_Name, Client_Listener_PID}, Client_List),
			Server_Input_Listener_PID ! {update_client_list, New_Client_List},
			handle_server_listening(Server_Input_Listener_PID, Server_Name, New_Client_List);

		% Output message from the client
        {client_message, Client_Name, Client_Input} ->
            io:format("~s: ~s", [Client_Name, Client_Input]),
			broadcast_message(Client_Name, Client_Input, Client_List),
            handle_server_listening(Server_Input_Listener_PID, Server_Name, Client_List)
    end.

% ─────────────────────────────────────────────────────────────────────────────────────
% Function to broadcast message to all clients except the sender
broadcast_message(Client_Name, Message, Client_List) ->
	% Loop through the list for clients
    lists:foreach(
        fun({Other_Client_Name, Other_Client_PID}) ->
            if 
                Other_Client_Name =/= Client_Name ->
					% Disconnect 
                    if Message =:= bye ->
                        Other_Client_PID ! bye;
					Message =:= user_connected ->
						Other_Client_PID ! {user_connected, Client_Name};
					Message =:= user_disconnected ->
						Other_Client_PID ! {user_disconnected, Client_Name};
                    true ->
                        Other_Client_PID ! {server_message, Client_Name, Message}
                    end;
                true -> ok
            end
        end,
        Client_List
    ).

% ─────────────────────────────────────────────────────────────────────────────────────
% Prompt for server inputs
handle_server_input(Server_Name, Client_List) ->
    % Spawn a separate process to get inputs from the user (server)
    Input_PID = spawn(?MODULE_NAME, handle_server_input_prompt, [Server_Name, self()]),

    % Start looping to monitor both user input and client list updates
    handle_server_input_listening(Server_Name, Client_List, Input_PID).

% ─────────────────────────────────────────────────────────────────────────────────────
% Main loop that handles both input and updates
handle_server_input_listening(Server_Name, Client_List, Input_PID) ->
    receive
        % When the client list is updated
        {update_client_list, Updated_Client_List} ->
            handle_server_input_listening(Server_Name, Updated_Client_List, Input_PID);

        % When receiving inputs from handle_server_input_prompt process
        {server_input, Server_Input} ->
            case string:lowercase(Server_Input) of
				% Terminate the process and force exit all users/clients' terminals
                "bye\n" ->
                    io:format("You ended the chat session.~n"),
                    broadcast_message(Server_Name, bye, Client_List),
                    erlang:halt();   % Force exit the erlang shell
                _ ->
                    % Broadcast the message to all clients
                    broadcast_message(Server_Name, Server_Input, Client_List),
                    handle_server_input_listening(Server_Name, Client_List, Input_PID)
            end
    end.

% ─────────────────────────────────────────────────────────────────────────────────────
% Separate process to handle continuously get inputs from the user
handle_server_input_prompt(Server_Name, Parent_PID) ->
	% Ask for user input
	Chat_Tag = io_lib:format("~s: ", [Server_Name]),
    Server_Input = io:get_line(Chat_Tag),
    Parent_PID ! {server_input, Server_Input},
    handle_server_input_prompt(Server_Name, Parent_PID).


% ═════════════════════════════════════════════════════════════════════════════════════
% Initialize the client processs
init_chat2(Chat_Node) ->
	% Check first if the client can ping the server
	case net_adm:ping(Chat_Node) of
		% Success
		pong -> 
			% Check first if Server is already initialized
			case rpc:call(Chat_Node, erlang, whereis, [server_listener]) of
        		undefined ->
					io:format("────────────────────────────────~n"), 
					io:format("Cannot establish a connection.~n"),
					io:format("init_chat() is not yet invoked.~n"),
					io:format("────────────────────────────────~n"),
					exit(normal);
				_ -> 
					Client_Name = string:trim(io:get_line("Enter Your Name: ")),
					Client_Listener_PID = spawn(?MODULE_NAME, handle_client_listening, []),
					{server_listener, Chat_Node} ! {connection_established, Client_Listener_PID, Client_Name},
					io:format("────────────────────────────────~n"), 
					io:format("Connection has been established.~n"),
					io:format("────────────────────────────────~n"),
					handle_client_input(Client_Name, Client_Listener_PID, Chat_Node)
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
            io:format("Server has ended the chat session.~n"),
			erlang:halt(); % Force exit the erlang shell

		% Output message from the server 
		{server_message, Server_Name, Server_Input} ->
			io:format("~s: ~s", [Server_Name, Server_Input]),
			handle_client_listening();

		% Output a message when a user connects
		{user_connected, User_Name} ->
			io:format("~s has joined the chat.~n", [User_Name]),
			handle_client_listening();

		% Output a message when a user disconnects
		{user_disconnected, User_Name} ->
			io:format("~s has left the chat.~n", [User_Name]),
			handle_client_listening()
	end.

% ─────────────────────────────────────────────────────────────────────────────────────
% Prompt for client inputs
handle_client_input(Client_Name, Client_Listener_PID, Chat_Node) ->
	% Ask for user input
	Chat_Tag = io_lib:format("~s: ", [Client_Name]),
    Client_Input = io:get_line(Chat_Tag),

    case string:lowercase(Client_Input) of
		% Exit if "bye"
        "bye\n" ->
			io:format("You have disconnected!~n"),
			{server_listener, Chat_Node} ! {bye, Client_Name, Client_Listener_PID},
			erlang:halt(); % Force exit the erlang shell
        _ ->
			% Send the message to the server
			{server_listener, Chat_Node} ! {client_message, Client_Name, Client_Input},
            handle_client_input(Client_Name, Client_Listener_PID, Chat_Node)
    end.
% ═════════════════════════════════════════════════════════════════════════════════════