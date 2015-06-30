/*
 * Copyright (c) 2015 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSString.h>

void __infer_assume(int cond);

@interface A : NSObject {
    @public int x;
}

-(A*) getA;

@end

@implementation A

-(A*) getA {
    return [A new];
}

@end

@interface C : NSObject
@property (copy, nonnull) NSString *name;

@end

@implementation C

- (instancetype)initWithCoder:(NSString*)aDecoder and:( A*  __nonnull)a
{
    A* a1 = [a getA];
    int y = a1->x;
    return self;
}

@end

void test(void (^ __nonnull callback)(NSError *, id)) {
    callback(NULL, NULL);
}
