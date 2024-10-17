SERVER_NAME = juliet
CLIENT_NAME = romeo
CLIENT_NAME2 = earl

COOKIE = amogus
HOSTNAME = 127.0.0.1

SERVER = ${SERVER_NAME}@${HOSTNAME}
CLIENT = ${CLIENT_NAME}@${HOSTNAME}
CLIENT2 = ${CLIENT_NAME2}@${HOSTNAME}

server: compile
	erl -name $(SERVER) -setcookie $(COOKIE)

client: compile
	erl -name $(CLIENT) -setcookie $(COOKIE) -eval "net_adm:ping('$(SERVER)')." 

client2: compile
	erl -name $(CLIENT2) -setcookie $(COOKIE) -eval "net_adm:ping('$(SERVER)')." 

compile: gabinete.beam

gabinete.beam: gabinete.erl
	erlc gabinete.erl
