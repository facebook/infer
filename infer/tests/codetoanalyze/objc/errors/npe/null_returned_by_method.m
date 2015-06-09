/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject

@end

@implementation A {
   int x;
}

-(A*)test {
   return nil;
}

-(int)test1 {
   return [self test]->x;
}

@end
