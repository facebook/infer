/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject

@end

@implementation A

- (void) testPrettyFunction {
    NSLog(@"%s", __PRETTY_FUNCTION__);
}

- (void) testFunction {
    NSLog(@"%s", __FUNCTION__);
}

- (void) testFunct {
    NSLog(@"%s", __func__);
}

@end
