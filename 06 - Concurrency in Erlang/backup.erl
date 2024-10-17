% % DESCRIPTION: 
% %       This is a simple Erlang program designed to mimic a live chat application.
% % AUTHOR:   Keith Ginoel S. Gabinete
% % CREATED:  October 16, 2024
% % ═════════════════════════════════════════════════════════════════════════════════════

% -module(chat).
% -compile([nowarn_export_all,export_all]).

% init_chat() ->
% 	register (chat_server, spawn(chat,server,[])).

% server() ->
% 	receive
% 		finished ->
% 			io:format("Server finished ~n");
% 		{ping, Client_Pid} ->
% 			io:format("Server got ping by Client~n"),
% 			Client_Pid ! pong,
% 			server()
% 	end.

% init_chat2(Chat_Node) ->
% 	spawn(chat, client, [3,Chat_Node]).

% client(0, Chat_Node) ->
% 	{chat_server, Chat_Node} ! finished,
% 	io:format("Client finished ~n");
% client(N, Chat_Node) ->
% 	{chat_server, Chat_Node} ! {ping, self()},
% 	receive
% 		pong ->
% 			io:format("Client got pong by Server~n")
% 	end,
% 	client(N-1,Chat_Node).