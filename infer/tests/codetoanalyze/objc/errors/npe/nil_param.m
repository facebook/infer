/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject {
  int x;
}
@end

@implementation A

-(void) test2 {
  self->x = 1;
}

-(void) test1:(A*) other {

        [other test2];


}

@end

int main () {

    A* a=[A alloc];
    [a test1: nil];
    [a release];
    return 0;
}
