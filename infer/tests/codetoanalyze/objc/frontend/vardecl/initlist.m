/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

int main() {
  int z;
  int a[2][3] = {{z+1, 2, 3}, {5,6,7}};
}

@interface C : NSObject

@end

int test() {
    C *c1 = [C alloc];
    C *c2 = [C alloc];
  C* a[3] = {[c1 init], c1, c2};
}
