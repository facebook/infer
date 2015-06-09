M_SOURCES=$(shell find . -name "*.m")
C_SOURCES=$(shell find . -name "*.c")
M_OBJECTS=$(M_SOURCES:.m=.o)
C_OBJECTS=$(C_SOURCES:.c=.o)
CC=clang
XCODE_PATH=$(shell xcode-select -p)
IPHONE_SIMULATOR_PATH=$(XCODE_PATH)/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk
FLAGS=-x objective-c -c -mios-simulator-version-min=8.2 -isysroot $(IPHONE_SIMULATOR_PATH)

default: test

all: test

.PHONY: test
test: $(C_OBJECTS) $(M_OBJECTS)
	echo "test called\n"

.PHONY: configure
configure:
	echo "no configure required\n"

clean:
	rm -rf $(M_OBJECTS)
	rm -rf $(C_OBJECTS)

.m.o:
	$(CC) $(FLAGS)  $< -o $@

.c.o:
	$(CC) $(FLAGS)  $< -o $@
