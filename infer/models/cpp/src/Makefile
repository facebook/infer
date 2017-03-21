SOURCES=$(wildcard *.cpp)
CSOURCES=$(wildcard c_src/*.c)
OBJECTS=$(patsubst %.c,%_cxx.o, $(CSOURCES)) $(patsubst %.cpp,%.o, $(SOURCES))
CXX=clang++
CXXFLAGS=-c -w -std=c++11 -isystem../include

all: $(OBJECTS)

clean:
	@rm -rf $(OBJECTS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

%_cxx.o: %.c
	$(CXX) $(CXXFLAGS) $< -o $@

.PHONY: all clean
