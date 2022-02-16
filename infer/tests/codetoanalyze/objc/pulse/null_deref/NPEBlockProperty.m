/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@class ExplicitCaptured;

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

// Needs analysis-time specialization
int test_explicitCaptured_specialized_bad_FN() {
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
