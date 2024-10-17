SERVER_NAME = server
CLIENT_NAME = client
HOSTNAME = ksgabinete

server: compile
	erl -sname $(SERVER_NAME)

client: compile
	erl -sname $(CLIENT_NAME) -eval "net_adm:ping('$(SERVER_NAME)@$(HOSTNAME)')."

compile: pingpong.beam

pingpong.beam: pingpong.erl
	erlc pingpong.erl
