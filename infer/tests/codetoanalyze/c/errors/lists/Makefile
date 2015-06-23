
SOURCES = $(shell ls *.c)
OBJECTS = $(SOURCES:.c=.o)

all: clean $(OBJECTS)
	echo $(OBJECTS)

.c.o:
	${CC} -c $<

clean:
	rm -rf $(OBJECTS)
