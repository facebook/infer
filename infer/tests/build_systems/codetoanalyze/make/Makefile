SOURCES = $(shell ls *.c)
OBJECTS = $(SOURCES:.c=.o)

all: $(OBJECTS)

.c.o:
	$(CC) -c $<

clean:
	@rm -rf $(OBJECTS)
