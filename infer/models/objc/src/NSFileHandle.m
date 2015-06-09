#import <Foundation/NSObject.h>
#include <unistd.h>

@class NSString, NSData, NSError;

@interface NSFileHandle : NSObject

- (void)closeFile;

- (instancetype)initWithFileDescriptor:(int)fd closeOnDealloc:(BOOL)closeopt;

- (instancetype)initWithFileDescriptor:(int)fd;

@property (readonly) int fileDescriptor;

@end

@implementation NSFileHandle

- (instancetype)initWithFileDescriptor:(int)fd closeOnDealloc:(BOOL)closeopt {
    if (self) {
        self->_fileDescriptor = fd;
        return self;
    }
}

- (instancetype)initWithFileDescriptor:(int)fd {
    [self initWithFileDescriptor:fd closeOnDealloc:NO];
}

- (void)closeFile {
    close(self->_fileDescriptor);
}

- (void)dealloc {
    [self closeFile];
}

@end
