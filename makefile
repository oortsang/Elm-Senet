CC = elm make
FILE = src/Main.elm
OUTPUT = Senet.js

elmake:
	$(CC) $(FILE) --output=$(OUTPUT)
