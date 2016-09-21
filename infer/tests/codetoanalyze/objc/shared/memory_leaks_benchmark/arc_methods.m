/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface ArcMethodsA : NSObject

+ (ArcMethodsA*)newA;

+ (ArcMethodsA*)someA;

@end

@implementation ArcMethodsA

+ (ArcMethodsA*)newA {
  ArcMethodsA* a = [[ArcMethodsA alloc] init];
  return a;
}

+ (ArcMethodsA*)someA {
  ArcMethodsA* a = [[ArcMethodsA alloc] init];

  return a;
}

@end

int main_arc_methods() {

  //   A * __weak aWeakRef =0;
  //    A * __strong a1 =0;
  //    A * __unsafe_unretained anUnsafeUnretRef =0;
  //    A * __autoreleasing anAutoRelRef =0;

  ArcMethodsA* a1 = [ArcMethodsA newA];
  ArcMethodsA* aa = a1;
  ArcMethodsA* a2 = [ArcMethodsA someA];
  ArcMethodsA* ab = a2;
  return 0;
}
