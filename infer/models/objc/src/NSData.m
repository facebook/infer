#import <Foundation/NSObject.h>
#include <stdlib.h>

NSData* __objc_alloc(NSData*);

@interface NSData : NSObject

@property (readonly) const void *bytes;

+ (instancetype)dataWithBytesNoCopy:(void *)bytes length:(NSUInteger)length;
+ (instancetype)dataWithBytesNoCopy:(void *)bytes length:(NSUInteger)length freeWhenDone:(BOOL)b;

@end

@implementation NSData

+ (instancetype)dataWithBytesNoCopy:(void *)bytes length:(NSUInteger)length {
    return [NSData dataWithBytesNoCopy:bytes length:length freeWhenDone:YES];
}

+ (instancetype)dataWithBytesNoCopy:(void *)bytes length:(NSUInteger)length freeWhenDone:(BOOL)b {
    NSData* data = __objc_alloc(self);
    if (data) {
        data->_bytes = bytes;
        return data;
    }
    else return nil;
}

- (void) dealloc {
    if (self)
        free(self->_bytes);
}
@end
