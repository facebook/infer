/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
