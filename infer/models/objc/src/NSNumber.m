#import "NSNumber.h"


@implementation NSNumber

+(NSNumber *)numberWithInt:(int)value {
    // using alloc as the documentation doesn't say it may return nil
    NSNumber *number = [self alloc];
    return [number initWithInt:value];
}

- (id)initWithInt:(int)v {
    self->value = (double)v;
    return self;
}

+(NSNumber *)numberWithFloat:(float)value {
    // using alloc as the documentation doesn't say it may return nil
    NSNumber *number = [NSNumber alloc];
    return [number initWithInt:value];
}

- (id)initWithFloat:(float)v {
    self->value = (double)v;
    return self;
}

+(NSNumber *)numberWithDouble:(double)value {
    // using alloc as the documentation doesn't say it may return nil
    NSNumber *number = [self alloc];
    return [number initWithInt:value];
}

- (id)initWithDouble:(double)v {
    self->value = v;
    return self;
}

+(NSNumber *)numberWithBool:(BOOL)value {
    // using alloc as the documentation doesn't say it may return nil
    NSNumber *number = [self alloc];
    return [number initWithBool:value];
}

- (id)initWithBool:(BOOL)v {
    self->value = (double)v;
    return self;
}

@end
