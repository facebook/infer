/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface Configs : NSObject

@property (nonatomic, readonly) BOOL feature_x;
@property (nonatomic, readonly) BOOL feature_y;
@property (nonatomic, readonly) BOOL feature_z;
@property (nonatomic, readonly) BOOL stored_config;

@end

@implementation Configs

- (BOOL)feature_x {
  return NO;
}

- (BOOL)feature_y {
  return NO;
}

- (BOOL)feature_z {
  return NO;
}

- (BOOL)stored_config {
  return NO;
}

@end

void doSomething(void) {}

void doSomethingElse(void) {}

@interface ConfigGating : NSObject {
  Configs* configs;
}

@end

@implementation ConfigGating

// Simple gated call: doSomething is gated by feature_x=true
- (void)simple_gated_call_bad {
  if (configs.feature_x) {
    doSomething();
  }
}

// Blocklisted callee: should not be reported even when ungated
- (void)test_blocklisted {
  NSString* dummy = [NSString stringWithUTF8String:"hello"];
  (void)dummy;
  NSMutableArray* arr = [[NSMutableArray alloc] init];
  [arr addObject:@"item"];
  NSUInteger count = [arr count];
  (void)count;
  id val = [arr objectAtIndex:0];
  (void)val;
}

// Not gated: doSomething is called unconditionally
- (void)not_gated_ok {
  doSomething();
}

// Both branches gated by the same config (different polarities)
- (void)both_branches_gated_bad {
  if (configs.feature_y) {
    doSomething();
  } else {
    doSomethingElse();
  }
}

// After the join point, the guard is removed
- (void)join_removes_guard_ok {
  if (configs.feature_z) {
    doSomething();
  } else {
    doSomethingElse();
  }
  // After the join, code is NOT gated (both branches merged)
  doSomething();
}

// Config stored in a variable before being checked
- (void)config_in_variable_bad {
  BOOL flag = configs.stored_config;
  if (flag) {
    doSomething();
  }
}

@end
