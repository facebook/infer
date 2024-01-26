/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "A.h"
#import "B.h"

@implementation B {
  A* _a;
}

- (int)npe_no_bad_footprint_in_getter:(A*)a {
  int* p = nil;
  NSData* metadata = a.metadata; // Doesn't crash here with Bad_footprint
  return *p; // NPE
}

- (int)npe_no_bad_footprint_in_setter:(A*)a andMetadata:(NSData*)metadata {
  int* p = nil;
  a.metadata = metadata; // Doesn't crash here with Bad_footprint
  return *p; // NPE
}

- (instancetype)infer_field_get_spec:(NSData*)m {
  A* a = [A alloc];
  [a withMetadata:m]; // Doesn't crash here with Precondition_not_met, get
                      // correct spec with a.x=5
  self->_a = a;
  return self;
}

- (int)npe_no_precondition_not_met:(NSData*)m {
  [self infer_field_get_spec:m];
  if ([self->_a getX] == 5) {
    int* p_true = nil;
    return *p_true; // NPE
  } else {
    int* p_false = nil;
    return *p_false; // no NPE, because we know the value of a._x is 5 because
                     // we get the correct spec in the method
                     // infer_field_get_spec
  }
}

- (int)calling_method_with_block_parameters {
  int h = 10;
  int z = 10;
  [A foo:h
      and:^(int i) {
        self->x = i;
      }
      and_also:^(int i) {
        self->y = h + z;
      }
      and:@"Hi"];
  return self->y;
}

/* This is not working now because of aliasing problems. */
+ (int)calling_method_with_block_parameters_sets_fields_correctly_FP {
  B* b = [B new];
  [b calling_method_with_block_parameters];
  if (b->x + b->y == 42) {
    int* p = 0;
    return *p; //  NPE here, because we know that the values x and y
               //  are set correctly by calling blocks
  } else {
    int* p = 0;
    return *p; // and not here
  }
}

- (int)calling_c_function_with_block_parameters {
  int h = 10;
  int z = 10;
  c_function(
      ^(int i) {
        self->x = i;
      },
      ^(int i) {
        self->y = h + z;
      },
      @"Hi");
  return self->y;
}

/* This is not working now because of aliasing problems. */
+ (int)calling_c_function_with_block_parameters_sets_fields_correctly_FP {
  B* b = [B new];
  [b calling_c_function_with_block_parameters];
  if (b->x + b->y == 42) {
    int* p = 0;
    return *p; //  NPE here, because we know that the values x and y
               //  are set correctly by calling blocks
  } else {
    int* p = 0;
    return *p; // and not here
  }
}
@end
