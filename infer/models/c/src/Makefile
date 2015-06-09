SOURCES=$(wildcard *.c)
OBJECTS=$(SOURCES:.c=.o)
CC=gcc
FLAGS=-c -w

default: test

all: test

.PHONY: test
test: $(OBJECTS) $(OBJECTSXX)
	echo "test called\n"

.PHONY: configure
configure:
	echo "no configure required\n"

clean:
	rm -rf *.o

.c.o:
	$(CC) $(FLAGS)  $< -o $@

