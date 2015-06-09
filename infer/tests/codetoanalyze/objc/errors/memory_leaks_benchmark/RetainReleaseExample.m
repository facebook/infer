/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject

@end

@implementation A

@end

void test() {
    A *a = [[A alloc] init];
    [a retain];
    [a release];
}
