/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject

+(A*) newA;

+(A*) someA;

@end

@implementation A

+ (A*) newA {
    A* a =[[A alloc] init];
    return a;
}

+ (A*) someA {
    A *a = [[A alloc] init];

    return a;
}

@end

int main () {

 //   A * __weak aWeakRef =0;
//    A * __strong a1 =0;
//    A * __unsafe_unretained anUnsafeUnretRef =0;
//    A * __autoreleasing anAutoRelRef =0;

    A *a1=[A  newA];
    A *aa = a1;
    A *a2 = [A someA];
    A *ab = a2;
    return 0;
}
