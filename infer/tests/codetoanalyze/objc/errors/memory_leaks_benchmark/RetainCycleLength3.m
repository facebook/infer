/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@class AA;
@class BB;
@class CC;

@interface AA : NSObject

@property(nonatomic, strong) BB* b;
@end

@implementation AA
@end

@interface BB : NSObject

@property(nonatomic, strong) CC* c;
@end

@implementation BB
@end

@interface CC : NSObject

@property(nonatomic, strong) AA* a;
@end

@implementation CC
@end

int strongcycle() {
  AA* a_obj = [AA new];
  BB* b_obj = [BB new];
  CC* c_obj = [CC new];
  a_obj.b = b_obj;
  b_obj.c = c_obj;
  c_obj.a = a_obj; // Retain cycle
  return 0;
}
