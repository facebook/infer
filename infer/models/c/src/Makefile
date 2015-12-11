SOURCES=$(wildcard *.c)
OBJECTS=$(SOURCES:.c=.o)
CC=gcc
CFLAGS=-c -w

all: $(OBJECTS) $(OBJECTSXX)

clean:
	@rm -rf $(OBJECTS)

.c.o:
	$(CC) $(CFLAGS) $< -o $@

.PHONY: all clean
