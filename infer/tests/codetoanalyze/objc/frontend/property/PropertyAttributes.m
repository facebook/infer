/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/Foundation.h>

@interface A : NSObject

@property (nonatomic, copy) A *child;

@property (nonatomic, retain) A *name;

@property (nonatomic, unsafe_unretained) A *last_name;

- (A*) copy;

@end

@implementation A

- (A*) copy {
    A *other = [[A alloc] init];
    if (other) {
        other->_name = self->_name;
        other->_last_name = self->_last_name;
        other->_child = self->_child;
    }
    return other;
}

@end

int test(A* a2) {
  A *a = [[A alloc] init];
  a.last_name = a2;
  [a release];
  return 0;
}
