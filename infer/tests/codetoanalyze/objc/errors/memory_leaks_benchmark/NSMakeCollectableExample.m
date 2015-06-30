#import <Foundation/Foundation.h>

@interface E : NSObject

@end

@implementation E

- (CFReadStreamRef) readStream {
    return nil;
}

- (void) handleStreamError
{
    NSError *underlyingError = NSMakeCollectable(
        [(NSError*)CFReadStreamCopyError((CFReadStreamRef)[self readStream]) autorelease]);
}

@end
