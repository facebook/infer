#import "NSString.h"
#import <stdlib.h>

void __get_array_size(const UInt8);

@implementation NSString 

+ (instancetype)stringWithUTF8String:(const char *)bytes {
    // using alloc as the documentation doesn't say it may return nil
    NSString *s = [NSString alloc];
    s->value = bytes;
    return s;
}

- (instancetype)initWithBytesNoCopy:(char*)bytes
                             length:(NSUInteger)length
                           encoding:(id)encoding
                       freeWhenDone:(BOOL)flag {
     if (flag == YES) {
        if (bytes)  {
            __get_array_size(bytes);
            free(bytes);
        }
     }
     return self;

}
@end

