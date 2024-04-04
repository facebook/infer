/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@class ExplicitCaptured;
@class RetainCycle;
@class Callback;

@interface ExplicitCaptured : NSObject

@property int explicit;
@property int (^get_explicitProperty)(void);
@property void (^set_explicitProperty)(int);

- (void)changeBehavior;

@end

@implementation ExplicitCaptured

- (instancetype)init {
  self = [super init];
  if (self != nil) {
    __weak __typeof__(self) weakSelf = self;
    self.get_explicitProperty = ^() {
      return weakSelf.explicit;
    };
    self.set_explicitProperty = ^(int x) {
      weakSelf.explicit = x;
    };
  }
  return self;
}

- (void)changeBehavior {
  __weak __typeof__(self) weakSelf = self;
  self.set_explicitProperty = ^(int x) {
    weakSelf.explicit += x;
  };
  self.get_explicitProperty = ^() {
    return weakSelf.explicit;
  };
}

@end

@interface RetainCycle : NSObject

@property RetainCycle* selfField;
@property int (^call_counter)(void);

@end

@implementation RetainCycle

- (instancetype)init {
  self = [super init];
  if (self != nil) {
    __block int counter = 0;
    self.call_counter = ^() {
      counter = counter + 1;
      return counter;
    };
  }
  return self;
}
@end

@interface Callback : NSObject

@property void (^callback)(void);

- (instancetype)initWithBlock:(void (^)(void))callback;

@end

@implementation Callback

- (instancetype)initWithBlock:(void (^)(void))callback {
  if (self = [super init]) {
    _callback = callback;
  }
  return self;
}
@end

int test_explicitCaptured_no_specialization_bad() {
  ExplicitCaptured* a = [ExplicitCaptured new];
  a.set_explicitProperty(0);
  int explicit = a.get_explicitProperty(); // explicit = 0
  a.set_explicitProperty(explicit + 1);
  explicit = a.get_explicitProperty(); // explicit = 1
  a.set_explicitProperty(explicit + 1);
  explicit = a.get_explicitProperty(); // explicit = 2
  a.set_explicitProperty(explicit + 1);
  explicit = a.get_explicitProperty(); // explicit = 3
  int* ptr = &explicit;
  if (explicit == a.explicit && explicit == 3) {
    ptr = NULL;
  }
  return *ptr; // Null deref here
}

int test_explicitCaptured_specializable(ExplicitCaptured* a) {
  int explicit = a.get_explicitProperty();
  a.set_explicitProperty(explicit + 1);
  return explicit;
}

int test_explicitCaptured_specialized_no_alias_bad_FP() {
  ExplicitCaptured* a = [ExplicitCaptured new];
  ExplicitCaptured* b = [ExplicitCaptured new];
  b.get_explicitProperty = a.get_explicitProperty;
  b.set_explicitProperty = a.set_explicitProperty;
  b.set_explicitProperty(0);
  int explicit = test_explicitCaptured_specializable(
      b); // explicit = 0; a.explicit = explicit + 1 = 1
  [a changeBehavior];
  b.get_explicitProperty = a.get_explicitProperty;
  b.set_explicitProperty = a.set_explicitProperty;
  explicit = test_explicitCaptured_specializable(
      b); // explicit = 1; a.explicit += (explicit + 1) -> 3
  int* ptr = &explicit;
  if (explicit == 1 && a.explicit == 3) {
    ptr = NULL;
  }
  return *ptr; // Null deref here
}

// needs alias'ed disjunct
int test_explicitCaptured_specialized_with_alias_bad() {
  ExplicitCaptured* a = [ExplicitCaptured new];
  a.set_explicitProperty(0);
  int explicit = test_explicitCaptured_specializable(
      a); // explicit = 0; a.explicit = explicit + 1 = 1
  [a changeBehavior];
  explicit = test_explicitCaptured_specializable(
      a); // explicit = 1; a.explicit += (explicit + 1) -> 3
  int* ptr = &explicit;
  if (explicit == 1 && a.explicit == 3) {
    ptr = NULL;
  }
  return *ptr; // Null deref here
}

int call_counter_3times(RetainCycle* rc) {
  rc.call_counter();
  rc.call_counter();
  return rc.call_counter();
}

int test_retainCycle_unused_cycle_bad() {
  RetainCycle* rc = [RetainCycle new];
  rc.selfField = rc;
  int counter = call_counter_3times(rc);
  int* ptr = &counter;
  if (counter == 3) {
    ptr = NULL;
  }
  return *ptr;
}

int test_retainCycle_unused_cycle_good_FP() {
  RetainCycle* rc = [RetainCycle new];
  rc.selfField = rc;
  int counter = call_counter_3times(rc);
  int* ptr = &counter;
  if (counter != 3) {
    ptr = NULL;
  }
  rc.selfField = nil; // to avoid a RETAIN_CYCLE issue
  return *ptr;
}

int call_sub_counters(RetainCycle* rc) {
  rc.call_counter();
  rc.selfField.call_counter();
  return rc.selfField.selfField.call_counter();
}

// needs aliasing
// needs retain cycle handled in specialization
int test_retainCycle_used_cycle_bad() {
  RetainCycle* rc = [RetainCycle new];
  rc.selfField = rc;
  int counter = call_sub_counters(rc);
  int* ptr = &counter;
  if (counter == 3) {
    ptr = NULL;
  }
  return *ptr;
}

int call_block_captured_in_captured(int (^block)(void)) { return block(); }

int captured_in_captured_specializable(RetainCycle* rc) {
  RetainCycle* rc2 = rc;
  int (^block)(void) = ^{
    return rc2.call_counter();
  };
  return call_block_captured_in_captured(block);
}

int test_captured_in_captured_specialized_bad() {
  RetainCycle* rc = [RetainCycle new];
  int x = 0;
  __block int* ptr = &x;
  rc.call_counter = ^{
    ptr = NULL;
    return x;
  };
  int y = 1;
  y = captured_in_captured_specializable(rc);
  if (x != y) { // should not happen
    ptr = &y;
  }
  return *ptr;
}

int test_captured_in_captured_specialized_good() {
  RetainCycle* rc = [RetainCycle new];
  int x = 0;
  int* ptr = &x;
  rc.call_counter = ^{
    return x;
  };
  int y = 1;
  y = captured_in_captured_specializable(rc);
  if (x != y) { // should not happen
    *ptr = NULL;
  }
  return *ptr;
}

void call_callback(Callback* cb) { cb.callback(); }

int test_localization_bad() {
  ExplicitCaptured* b = [ExplicitCaptured new];
  ExplicitCaptured* a = [ExplicitCaptured new];
  a.get_explicitProperty = b.get_explicitProperty;
  a.set_explicitProperty = b.set_explicitProperty;
  Callback* cb = [[Callback alloc] initWithBlock:^{
    a.set_explicitProperty(3);
  }];
  call_callback(cb);
  int explicit = a.get_explicitProperty();
  int* ptr = &explicit;
  if (explicit == 3) {
    ptr = NULL;
  }
  return *ptr;
}

int test_localization_good() {
  ExplicitCaptured* b = [ExplicitCaptured new];
  ExplicitCaptured* a = [ExplicitCaptured new];
  a.get_explicitProperty = b.get_explicitProperty;
  a.set_explicitProperty = b.set_explicitProperty;
  Callback* cb = [[Callback alloc] initWithBlock:^{
    a.set_explicitProperty(3);
  }];
  call_callback(cb);
  int explicit = a.get_explicitProperty();
  int* ptr = &explicit;
  if (explicit != 3) {
    ptr = NULL;
  }
  return *ptr;
}
