/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@class AA;
@class BB;

@interface AA : NSObject

@property(nonatomic, strong) BB* b;
@end

@implementation AA
@end

@interface BBStrong : NSObject

@property(nonatomic, strong) AA* a;
@end

@implementation BBStrong
@end

@interface BBUnsafeUnretained : NSObject

@property(nonatomic, unsafe_unretained) AA* a;
@end

@implementation BBUnsafeUnretained
@end

@interface BBWeak : NSObject

@property(nonatomic, weak) AA* a;
@end

@implementation BBWeak
@end

@interface BBAssign : NSObject

@property(nonatomic, assign) AA* a;
@end

@implementation BBAssign
@end

int strongcycle_bad() {

  AA* a_obj = [AA alloc];
  BBStrong* b_obj = [BBStrong alloc];

  a_obj.b = b_obj;
  b_obj.a = a_obj; // Retain cycle

  return 0;
}

int unsafeunreainedcycle_good() {

  AA* a_obj = [AA alloc];
  BBUnsafeUnretained* b_obj = [BBUnsafeUnretained alloc];

  a_obj.b = b_obj;
  b_obj.a = a_obj; // Not a retain cycle

  return 0;
}

int weakcycle_good() {

  AA* a_obj = [AA alloc];
  BBWeak* b_obj = [BBWeak alloc];

  a_obj.b = b_obj;
  b_obj.a = a_obj; // Not a retain cycle

  return 0;
}

int assigncycle_good() {

  AA* a_obj = [AA alloc];
  BBAssign* b_obj = [BBAssign alloc];

  a_obj.b = b_obj;
  b_obj.a = a_obj; // Not a retain cycle

  return 0;
}
