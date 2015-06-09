#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#pragma clang diagnostic ignored "-Wdeprecated-objc-isa-usage"

#import "NSMutableArray.h"

@implementation NSMutableArray

- (void)setObject:(id)anObject atIndexedSubscript:(NSUInteger)index {
    NSObject *obj = (NSObject*)anObject;
    id isa = obj->isa;
}

- (void)addObject:(id)anObject {
    NSObject *obj = (NSObject*)anObject;
    id isa = obj->isa;
}

- (void)insertObject:(id)anObject atIndex:(NSUInteger)index {
    NSObject *obj = (NSObject*)anObject;
    id isa = obj->isa;
}

+ (instancetype)array {
    return [NSMutableArray alloc];
}


@end
