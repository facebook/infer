/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@interface MyClass : NSObject

@end

@implementation MyClass

- (void) aMethod {
    int i = 0;
    int j = 0;
    if (i == 0) {
        return;
    }

    if(j == 0) {
        i++;
    }
}

@end
