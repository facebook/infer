#import <Foundation/NSArray.h>

@interface A : NSObject
@end

@implementation A

- (int) fast_loop: (NSArray *) items {
    int size = 0;
    for (NSArray* item in items) {
        size += [item count];
    }
    return size;
}

- (int) while_loop: (NSArray*) items {
    int size = 0;
    NSArray* item = nil;
    while (item = [items nextObject]) {
        size += [item count];
    }
    return size;
}

@end
