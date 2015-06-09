/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@implementation A

-(instancetype) init
{
    if ([super self]) {
        self->x = 10;
    }
    return self;
}

@end
