SERVER_NAME = juliet
CLIENT_NAME1 = romeo
CLIENT_NAME2 = juan
CLIENT_NAME3 = maria
CLIENT_NAME4 = pedro

COOKIE = amogus
HOSTNAME = 127.0.0.1

SERVER = ${SERVER_NAME}@${HOSTNAME}
CLIENT1 = ${CLIENT_NAME1}@${HOSTNAME}
CLIENT2 = ${CLIENT_NAME2}@${HOSTNAME}
CLIENT3 = ${CLIENT_NAME3}@${HOSTNAME}
CLIENT4 = ${CLIENT_NAME4}@${HOSTNAME}

s: compile
	erl -name $(SERVER) -setcookie $(COOKIE)

c1: compile
	erl -name $(CLIENT1) -setcookie $(COOKIE) -eval "net_adm:ping('$(SERVER)')."

c2: compile
	erl -name $(CLIENT2) -setcookie $(COOKIE) -eval "net_adm:ping('$(SERVER)')."

c3: compile
	erl -name $(CLIENT3) -setcookie $(COOKIE) -eval "net_adm:ping('$(SERVER)')."

c4: compile
	erl -name $(CLIENT4) -setcookie $(COOKIE) -eval "net_adm:ping('$(SERVER)')."

compile: gabinete.beam gabinete_bonus.beam

gabinete.beam: gabinete.erl
	erlc gabinete.erl

gabinete_bonus.beam: gabinete_bonus.erl
	erlc gabinete_bonus.erl
