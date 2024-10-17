SERVER_NAME = juliet
CLIENT_NAME = romeo

COOKIE = amogus
HOSTNAME = 127.0.0.1

SERVER = ${SERVER_NAME}@${HOSTNAME}
CLIENT = ${CLIENT_NAME}@${HOSTNAME}

server: compile
	erl -name $(SERVER) -setcookie $(COOKIE)

client: compile
	erl -name $(CLIENT) -setcookie $(COOKIE) -eval "net_adm:ping('$(SERVER)')." 

compile: chat.beam

chat.beam: chat.erl
	erlc chat.erl
