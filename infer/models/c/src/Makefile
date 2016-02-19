SOURCES=$(wildcard *.c)
OBJECTS=$(SOURCES:.c=.o)
CC=clang
CFLAGS=-c -w

all: $(OBJECTS) 

clean:
	@rm -rf $(OBJECTS) 

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

.PHONY: all clean
