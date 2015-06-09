/*
 * Copyright (c) 2014- Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

@interface A : NSObject {
    int x;
}
@property A *son;
@end

@implementation A

/* autorelease is added */
-(NSString*) getS{
    NSString *s = [NSString alloc];
    return s;
}

/* autorelease is not added */
-(NSString*) newS{
    NSString *s = [NSString alloc];
    return s;
}

@end
