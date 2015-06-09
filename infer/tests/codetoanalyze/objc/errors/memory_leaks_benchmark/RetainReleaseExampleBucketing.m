/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject

@end

@implementation A

@end

// no leak in bucketing cf mode
void test() {
    A *a = [[A alloc] init];
}
