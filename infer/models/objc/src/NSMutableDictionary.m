#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#pragma clang diagnostic ignored "-Wdeprecated-objc-isa-usage"

#import "NSMutableDictionary.h"

@implementation NSMutableDictionary

- (void)setObject:(id)object forKeyedSubscript:(id)aKey {
    NSObject *obj = (NSObject*)object;
    id isa = obj->isa;
    NSObject *key = (NSObject*)aKey;
    id isa2 = key->isa;
}

- (void)setObject:(id)anObject forKey:(id)aKey {
    NSObject *obj = (NSObject*)anObject;
    id isa = obj->isa;
    NSObject *key = (NSObject*)aKey;
    id isa2 = key->isa;
}

+ (instancetype)dictionary {
    return [NSMutableDictionary alloc];
}

@end
