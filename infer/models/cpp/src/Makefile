SOURCES=$(wildcard *.cpp)
OBJECTS=$(SOURCES:.cpp=.o)
CXX=g++
FLAGS=-c -w

default: test

all: test

.PHONY: test
test: $(OBJECTS)
	echo "test called\n"

.PHONY: configure
configure:
	echo "no configure required\n"

clean:
	rm -rf *.o

.c.o:
	$(CXX) $(FLAGS)  $< -o $@
