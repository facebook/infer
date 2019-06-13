/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject

@end

@implementation A

@end

int main() {

  A* __weak aWeakRef = 0;
  A* __strong aStrongRef = 0;
  A* __unsafe_unretained anUnsafeUnretRef = 0;
  A* __autoreleasing anAutoRelRef = 0;
  A* aStdRef = 0;

  //  interaction with __strong
  aStrongRef = [A alloc];
  // counter =1
  aStdRef = aStrongRef;
  // counter = 2
  aStrongRef = 0;
  // counter =1
  aWeakRef = aStdRef;
  // counter =1
  anAutoRelRef = aStdRef;
  // counter=2
  anUnsafeUnretRef = aStdRef;
  // counter=2

  return 0;
}
