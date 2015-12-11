SOURCES=$(wildcard *.cpp)
OBJECTS=$(SOURCES:.cpp=.o)
CXX=g++
CXXFLAGS=-c -w

all: $(OBJECTS)

clean:
	@rm -rf $(OBJECTS)

.c.o:
	$(CXX) $(CXXFLAGS) $< -o $@

.PHONY: all clean
