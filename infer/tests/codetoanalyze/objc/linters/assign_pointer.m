/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

struct MyStruct {
  int* i;
};

@protocol P

@property(assign) NSNumber* shouldReport1;

@property(atomic, assign) NSObject* shouldReport2;

@property(assign) id shouldReport3; // id's are pointers to Obj-C objects

@property(assign) int shouldNotReport1;

@property(atomic, assign) int shouldNotReport2;

@property(atomic, strong) NSNumber* shouldNotReport3;

@property(atomic) NSNumber* shouldNotReport4; // strong is the default attribute

@property(assign) int* shouldNotReport5; // assign for not-Obj-C pointers is ok

@property(assign) void* shouldNotReport6;

@property(assign) struct MyStruct* shouldNotReport7;

@property(nonatomic, assign, nullable) NSNumber* shouldReport8;

@end
