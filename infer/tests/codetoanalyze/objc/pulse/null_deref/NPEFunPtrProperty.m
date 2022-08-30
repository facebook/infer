/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@class WithFunPtrProperties;
@class FunPtrCallback;

@interface WithFunPtrProperties : NSObject

@property int explicit;
@property int (*get_explicitProperty)(WithFunPtrProperties*);
@property void (*set_explicitProperty)(WithFunPtrProperties*, int);

- (void)changeBehavior;

@end

int get_explicitProperty(WithFunPtrProperties* context) {
  return context.explicit;
}

void set_explicitProperty(WithFunPtrProperties* context, int x) {
  context.explicit = x;
}

@implementation WithFunPtrProperties

- (instancetype)init {
  self = [super init];
  if (self != nil) {
    self.get_explicitProperty = &get_explicitProperty;
    self.set_explicitProperty = &set_explicitProperty;
  }
  return self;
}

int set_explicitProperty2(WithFunPtrProperties* context, int x) {
  context.explicit += x;
}

- (void)changeBehavior {
  self.set_explicitProperty = &set_explicitProperty2;
}

@end

@interface FunPtrCallback : NSObject

@property void (*funPtrCallback)(NSObject*);

- (instancetype)initWithFunPtr:(void (*)(NSObject*))funPtrCallback;

@end

@implementation FunPtrCallback

- (instancetype)initWithFunPtr:(void (*)(NSObject*))funPtrCallback {
  if (self = [super init]) {
    _funPtrCallback = funPtrCallback;
  }
  return self;
}
@end

int test_withFunPtrProperties_no_specialization_bad() {
  WithFunPtrProperties* a = [WithFunPtrProperties new];
  (*a.set_explicitProperty)(a, 0);
  int explicit = (*a.get_explicitProperty)(a); // explicit = 0
  (*a.set_explicitProperty)(a, explicit + 1);
  explicit = (*a.get_explicitProperty)(a); // explicit = 1
  (*a.set_explicitProperty)(a, explicit + 1);
  explicit = (*a.get_explicitProperty)(a); // explicit = 2
  (*a.set_explicitProperty)(a, explicit + 1);
  explicit = (*a.get_explicitProperty)(a); // explicit = 3
  int* ptr = &explicit;
  if (explicit == a.explicit && explicit == 3) {
    ptr = NULL;
  }
  return *ptr; // Null deref here
}

int test_withFunPtrProperties_specializable(WithFunPtrProperties* a) {
  int explicit = (*a.get_explicitProperty)(a);
  (*a.set_explicitProperty)(a, explicit + 1);
  return explicit;
}

int test_withFunPtrProperties_specialized_no_alias_bad() {
  WithFunPtrProperties* a = [WithFunPtrProperties new];
  WithFunPtrProperties* b = [WithFunPtrProperties new];
  b.get_explicitProperty = a.get_explicitProperty;
  b.set_explicitProperty = a.set_explicitProperty;
  (*b.set_explicitProperty)(b, 0);
  int explicit = test_withFunPtrProperties_specializable(
      b); // explicit = 0; a.explicit = explicit + 1 = 1
  [a changeBehavior];
  b.get_explicitProperty = a.get_explicitProperty;
  b.set_explicitProperty = a.set_explicitProperty;
  explicit = test_withFunPtrProperties_specializable(
      b); // explicit = 1; a.explicit += (explicit + 1) -> 3
  int* ptr = &explicit;
  if (explicit == 1 && a.explicit == 3) {
    ptr = NULL;
  }
  return *ptr; // Null deref here
}

int test_withFunPtrProperties_specialized_with_alias_bad() {
  WithFunPtrProperties* a = [WithFunPtrProperties new];
  (*a.set_explicitProperty)(a, 0);
  int explicit = test_withFunPtrProperties_specializable(
      a); // explicit = 0; a.explicit = explicit + 1 = 1
  [a changeBehavior];
  explicit = test_withFunPtrProperties_specializable(
      a); // explicit = 1; a.explicit += (explicit + 1) -> 3
  int* ptr = &explicit;
  if (explicit == 1 && a.explicit == 3) {
    ptr = NULL;
  }
  return *ptr; // Null deref here
}

void call_funPtrCallback(FunPtrCallback* cb, NSObject* context) {
  (*cb.funPtrCallback)(context);
}

void set_explicitProperty_to_3(WithFunPtrProperties* context) {
  (*context.set_explicitProperty)(context, 3);
}

int funptr_test_localization_bad() {
  WithFunPtrProperties* b = [WithFunPtrProperties new];
  WithFunPtrProperties* a = [WithFunPtrProperties new];
  a.get_explicitProperty = b.get_explicitProperty;
  a.set_explicitProperty = b.set_explicitProperty;
  FunPtrCallback* cb =
      [[FunPtrCallback alloc] initWithFunPtr:&set_explicitProperty_to_3];
  call_funPtrCallback(cb, a);
  int explicit = (*a.get_explicitProperty)(a);
  int* ptr = &explicit;
  if (explicit == 3) {
    ptr = NULL;
  }
  return *ptr;
}

int funptr_test_localization_good() {
  WithFunPtrProperties* b = [WithFunPtrProperties new];
  WithFunPtrProperties* a = [WithFunPtrProperties new];
  a.get_explicitProperty = b.get_explicitProperty;
  a.set_explicitProperty = b.set_explicitProperty;
  FunPtrCallback* cb =
      [[FunPtrCallback alloc] initWithFunPtr:&set_explicitProperty_to_3];
  call_funPtrCallback(cb, a);
  int explicit = (*a.get_explicitProperty)(a);
  int* ptr = &explicit;
  if (explicit != 3) {
    ptr = NULL;
  }
  return *ptr;
}
