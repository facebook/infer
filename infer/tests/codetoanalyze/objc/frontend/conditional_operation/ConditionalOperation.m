/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/Foundation.h>

@interface A : NSObject

-(int) test4:(int) x;
@end

@implementation A

-(int) test4:(int) x {
    return x;
}

-(int) test5:(BOOL) b {
    return [self test4 : (b ? b : 1)];
}

@end
