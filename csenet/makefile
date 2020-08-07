CC = gcc
OPT = -O0
WARNINGS = -Wall
DEBUG = -g3
CFLAGS = $(OPT) $(DEBUG) $(WARNINGS)
OBJS = main.o board.o logic.o movesearch.o
DEPS = main.h board.h logic.h movesearch.h
OUTPUT = senet

%.o: %.c
	$(CC) -c -o $@ $^ $(CFLAGS)

compile: $(OBJS)
	$(CC) -o $(OUTPUT) $(OBJS) $(DEPS) $(CFLAGS)
