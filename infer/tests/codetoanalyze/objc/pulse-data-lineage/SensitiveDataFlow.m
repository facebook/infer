/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface TaintedObject : NSObject
@end

@implementation TaintedObject
+ (TaintedObject*)__infer_taint_source {
  return [TaintedObject new];
}
@end

@interface SensitiveDataFlow : NSObject
@end

@implementation SensitiveDataFlow

- (NSObject*)__infer_taint_source {
  return [NSObject new];
}
- (NSObject*)__infer_taint_sanitizer:(NSObject*)obj {
  return obj;
}

- (void)might_be_a_sink:(NSObject*)obj {
}

- (NSObject*)create_taint {
  return self.__infer_taint_source;
}

- (NSObject*)mutate:(NSObject*)obj {
  return obj;
}

- (NSObject*)create_then_mutate {
  NSObject* source = self.create_taint;
  source = [self mutate:source];
  return source;
}

- (void)consume:(NSObject*)obj {
  [self might_be_a_sink:obj];
}

- (void)mutate_then_consume:(NSObject*)obj {
  obj = [self mutate:obj];
  [self consume:obj];
}

- (void)test {
  NSObject* start = self.create_then_mutate;
  [self mutate_then_consume:start];
}

- (void)test_via_sanitizer {
  NSObject* obj = self.__infer_taint_source;
  obj = [self __infer_taint_sanitizer:obj];
  [self might_be_a_sink:obj];
}

NSObject* unknown(NSObject*);

- (NSObject*)propagate_taint:(NSObject*)obj {
  return unknown(obj);
}

- (void)test_flow_to_unknown {
  NSObject* obj = self.__infer_taint_source;
  unknown(obj);
}

- (void)test_taint_propagation {
  NSObject* obj = self.__infer_taint_source;
  NSObject* ret = [self propagate_taint:obj];
  [self might_be_a_sink:ret];
}

- (void)test_ignored_calls {
  TaintedObject* tainted = TaintedObject.__infer_taint_source;
  [self might_be_a_sink:tainted];
}

@end
