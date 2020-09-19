/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface Basic : NSObject
@end

@implementation Basic

- (void)call_autorelease_constant {
  NSString* x = [[NSString alloc] initWith:"test"];
  return [x autorelease];
}

- (void)autoreleased_in_loop_linear:(int)n {
  for (int i = 0; i < n; i++) {
    [self call_autorelease_constant];
  }
}

- (void)autoreleased_in_autoreleasepool_zero:(int)n {
  for (int i = 0; i < n; i++) {
    @autoreleasepool {
      [self call_autorelease_constant];
    }
  }
}

/* This function is problematic because autoreleased objects inside the loop
   will not be released until the loop terminates.  However, the checker is not
   measuring peak memory but size of autoreleasepool increased by the function.
   Since we wrap the loop inside the autoreleasepool block, we will be setting
   the size to 0 for the whole function. */
- (void)loop_in_autoreleasepool_zero:(int)n {
  @autoreleasepool {
    for (int i = 0; i < n; i++) {
      [self call_autorelease_constant];
    }
  }
}

- (void)autoreleased_in_loop_nested_zero:(int)n {
  @autoreleasepool {
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        [self call_autorelease_constant];
      }
      @autoreleasepool {
        [self call_autorelease_constant];
      }
    }
  }
}

- (void)autoreleased_in_loop_sequential_constant:(int)n {
  @autoreleasepool {
    for (int i = 0; i < n; i++) {
      [self call_autorelease_constant];
    }
  }
  [self call_autorelease_constant];
  @autoreleasepool {
    for (int i = 0; i < n; i++) {
      [self call_autorelease_constant];
    }
  }
}

- (void)autoreleased_in_loop_sequential_linear:(int)n {
  @autoreleasepool {
    [self call_autorelease_constant];
  }
  for (int i = 0; i < n; i++) {
    [self call_autorelease_constant];
  }
  @autoreleasepool {
    [self call_autorelease_constant];
  }
}

- (void)call_no_autorelease_zero {
  NSString* x = [[NSString alloc] initWith:"test"];
  return x;
}

- (void)no_autoreleased_in_loop_zero:(int)n {
  for (int i = 0; i < n; i++) {
    [self call_no_autorelease];
  }
}

- (void)autorelease_unreachable_zero:(int)n {
  int i = 1;
  if (i == 0) {
    for (int j = 0; j < n; j++) {
      [self call_autorelease_constant];
    }
  }
}

- (void)multiple_autorelease_constants:(int)n {
  [self call_autorelease_constant];
  for (int i = 0; i < n; i++) {
    [self call_autorelease_constant];
  }
  [self call_autorelease_constant];
}

- (void)call_cf_autorelease_constant:(NSObject*)x {
  CFAutorelease((__bridge CFTypeRef)x);
}

@end
