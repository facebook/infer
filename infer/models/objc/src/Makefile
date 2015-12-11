M_SOURCES=$(shell find . -name "*.m")
C_SOURCES=$(shell find . -name "*.c")
OBJECTS=$(M_SOURCES:.m=.o) $(C_SOURCES:.c=.o)
CC=clang
XCODE_PATH=$(shell xcode-select -p)
IPHONE_SIMULATOR_PATH=$(XCODE_PATH)/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk
CFLAGS=--target=x86_64-apple-darwin14 -x objective-c -c -mios-simulator-version-min=8.2 -isysroot $(IPHONE_SIMULATOR_PATH)

all: $(OBJECTS)

clean:
	@rm -rf $(OBJECTS)

%.o: %.m
	$(CC) $(CFLAGS) $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

.PHONY: all clean
