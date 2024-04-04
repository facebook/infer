/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@class ClassA;
@class ClassB;
@class ClassC;

@class SelfReferencing;

@interface ClassA : NSObject

@property(nonatomic, strong) ClassB* b;
@end

@implementation ClassA
@end

@interface ClassBWeak : NSObject

@property(nonatomic, weak) ClassA* a;
@property int* allocated;
@end

@implementation ClassBWeak
- (instancetype)init {
  if (self = [super init]) {
    _allocated = malloc(sizeof(int));
  }
  return self;
}

- (void)dealloc {
  free(_allocated);
}
@end

@interface ClassBStrong : NSObject

@property(nonatomic, strong) ClassA* a;
@property int* allocated;
@end

@implementation ClassBStrong
- (instancetype)init {
  if (self = [super init]) {
    _allocated = malloc(sizeof(int));
  }
  return self;
}

- (void)dealloc {
  free(_allocated);
}
@end

@interface ClassC : NSObject

@property(nonatomic, strong) ClassA* a;
@end

@implementation ClassC
@end

@interface SelfReferencing : NSObject

@property(nonatomic, strong) SelfReferencing* s;
@property int* allocated;
@end

@implementation SelfReferencing
- (instancetype)init {
  if (self = [super init]) {
    _allocated = malloc(sizeof(int));
  }
  return self;
}

- (void)dealloc {
  free(_allocated);
}
@end

void weak_cycle() {
  ClassA* a_obj = [ClassA alloc];
  ClassBWeak* b_obj = [[ClassBWeak alloc] init];

  a_obj.b = b_obj;
  b_obj.a = a_obj; // Not a retain cycle
}

void broken_strong_cycle_good_FP() {
  ClassA* a_obj = [ClassA alloc];
  ClassBStrong* b_obj = [[ClassBStrong alloc] init];

  a_obj.b = b_obj;
  b_obj.a = a_obj; // retain cycle
  // b_obj is removed from the stack here
  // but not deallocated because still reachable from a_obj
  a_obj.b = nil; // broken cycle
  // b_obj becomes unreachable but is not part of the stack so it does not get
  // a second chance at being deallocated, therefore it lives with its
  // malloc'ed content on the heap => memory leak
  // This is a FP because b_obj would actually receive a release message from
  // the reassignment of a_obj.b leading to its deallocation
}

void broken_inner_cycle_good_FP() {
  ClassC* c_obj = [ClassC alloc];
  c_obj.a = [ClassA alloc];
  ClassBStrong* b_obj = [[ClassBStrong alloc] init];
  b_obj.a = c_obj.a;
  c_obj.a.b = b_obj; // retain cycle
  // b_obj is removed from the stack here
  // but not deallocated because still reachable from c_obj.a
  c_obj.a.b = nil; // broken cycle
  // b_obj becomes unreachable but is not part of the stack so it does not get
  // a second chance at being deallocated, therefore it lives with its
  // malloc'ed content on the heap => memory leak
  // This is a FP because b_obj would actually receive a release message from
  // the reassignment of c_obj.a.b leading to its deallocation
}

void broken_self_strong_cycle_good_FP() {
  SelfReferencing* obj = [[SelfReferencing alloc] init];
  obj.s = obj; // retain cycle
  obj.s = nil; // broken cycle
}
