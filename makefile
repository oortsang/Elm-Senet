CC = elm make
FILE = src/Main.elm
OUTPUT = Senet.html

elmake: 
	$(CC) $(FILE) --output=$(OUTPUT)

