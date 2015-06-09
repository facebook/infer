/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject
@property int x;
@end

@implementation A

- (int)addTarget:(A*)target {
    return target.x;
}

@end
