/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/Foundation.h>

@interface A : NSObject
@property int x;
@end

@implementation A

- (int)addTarget:(A*)target {
    NSAssert(target != nil, @"target must not be nil");
    return target.x;
}

- (int)initWithRequest:(A*)a {
    NSAssert1(a != nil, @"target must not be nil %s", "a");
    return a.x;
}

@end

int test1(A* target) {
    NSCAssert(target != nil, @"target must not be nil");
    return target.x;
}

int test2(A* target) {
    NSCParameterAssert(target);
    return target.x;
}
