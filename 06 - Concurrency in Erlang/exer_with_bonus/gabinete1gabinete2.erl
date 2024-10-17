% DESCRIPTION: 
%       This is a simple Erlang program that allows two users to communicate with each 
% other (just like a chat application). One user will start the communication process 
% by invoking the init_chat function. The other user connects to the first user by
% invoking the init_chat2 function while specifying the first user's name (passing it as a parameter).
% The communication process terminates when one of the users sends the string 'bye'.
% AUTHOR:   Keith Ginoel S. Gabinete
% CREATED:  October 16, 2024
% ═════════════════════════════════════════════════════════════════════════════════════

-module(gabinete1gabinete2).
-compile([nowarn_export_all,export_all]).

% GLOBAL VARIABLE
-define(MODULE_NAME, gabinete1gabinete2).

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

	register(server_listener, spawn(?MODULE_NAME, handle_server_listening, [self(), Server_Name, [{Server_Name, self()}]])),
	handle_server_input(Server_Name, []).

% ─────────────────────────────────────────────────────────────────────────────────────
% A loop/recursive function for monitoring mails for the server process
handle_server_listening(Server_Input_Listener_PID, Server_Name, Client_List) ->
    receive
		{connection_established, Client_Listener_PID, Client_Name} ->
			Temp_Message = io_lib:format("CONNECTED...~n", []),
			io:format("~s: CONNECTED...~n", [Client_Name]),
			broadcast_message(Client_Name, Temp_Message, Client_List),
			New_Client_List = [{Client_Name, Client_Listener_PID} | Client_List],
			Server_Input_Listener_PID ! {update_client_list, New_Client_List},
			handle_server_listening(Server_Input_Listener_PID, Server_Name, New_Client_List);

        {bye, Client_Name, Client_Listener_PID} ->
			Temp_Message = io_lib:format("DISCONNECTED...~n", []),
			io:format("~s: DISCONNECTED...~n", [Client_Name]),
			broadcast_message(Client_Name, Temp_Message, Client_List),
			New_Client_List = lists:delete({Client_Name, Client_Listener_PID}, Client_List),
			Server_Input_Listener_PID ! {update_client_list, New_Client_List},
			handle_server_listening(Server_Input_Listener_PID, Server_Name, New_Client_List);
			% erlang:halt(); % Force exit the erlang shell

		% Output message from the client
        {client_message, Client_Name, Client_Input} ->
            io:format("~s: ~s", [Client_Name, Client_Input]),
			broadcast_message(Client_Name, Client_Input, Client_List),
            handle_server_listening(Server_Input_Listener_PID, Server_Name, Client_List)
    end.

% ─────────────────────────────────────────────────────────────────────────────────────
% Function to broadcast message to all clients except the sender
broadcast_message(Client_Name, Message, Client_List) ->
    lists:foreach(
        fun({Other_Client_Name, Other_Client_PID}) ->
            if 
                Other_Client_Name =/= Client_Name ->
                    if Message =:= bye ->
                        Other_Client_PID ! bye;
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
    % Set up to receive updated Client_List from server_listener
    receive
        {update_client_list, Updated_Client_List} ->
            handle_server_input(Server_Name, Updated_Client_List)
    after 0 ->
        % Ask for user input
        Server_Input = io:get_line("You: "),

        case string:lowercase(Server_Input) of
            % Exit if "bye"
            "bye\n" ->
                io:format("Server has disconnected!~n"),
                broadcast_message(Server_Name, bye, Client_List),
                erlang:halt();  % Stop the server and disconnect all clients
            _ ->
                % Send the message to the clients
                broadcast_message(Server_Name, Server_Input, Client_List),
                handle_server_input(Server_Name, Client_List)
        end
    end.


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
            io:format("Server has disconnected!~n"),
			erlang:halt(); % Force exit the erlang shell

		% Output message from the server 
		{server_message, Server_Name, Server_Input} ->
			io:format("~s: ~s", [Server_Name, Server_Input]),
			handle_client_listening()
	end.

% ─────────────────────────────────────────────────────────────────────────────────────
% Prompt for client inputs
handle_client_input(Client_Name, Client_Listener_PID, Chat_Node) ->
	% Ask for user input
    Client_Input = io:get_line("You: "),

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