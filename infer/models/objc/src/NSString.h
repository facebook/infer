#import <Foundation/NSObject.h>

@interface NSString : NSObject {
    const char * value;
}

+ (instancetype)stringWithUTF8String:(const char *)bytes;

@end
