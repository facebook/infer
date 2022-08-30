/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface InheritsNSObject : NSObject
@end

@implementation InheritsNSObject
@end

@interface DataFlowToSink : NSObject
@end

@implementation DataFlowToSink

- (NSObject*)might_be_a_source {
  return [NSObject new];
}

- (void)__infer_taint_sink:(NSObject*)obj {
}

- (NSObject*)__infer_taint_sanitizer:(NSObject*)obj {
  return obj;
}

- (NSObject*)sanitize:(NSObject*)obj {
  NSObject* sanitized = [self __infer_taint_sanitizer:obj];
  return sanitized;
}

- (NSObject*)create_taint {
  return self.might_be_a_source;
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
  [self __infer_taint_sink:obj];
}

- (void)mutate_then_consume:(NSObject*)obj {
  obj = [self mutate:obj];
  [self consume:obj];
}

- (void)test_simple {
  NSObject* start = self.create_then_mutate;
  [self mutate_then_consume:start];
}

- (void)test_through_sanitizer {
  NSObject* obj = self.create_then_mutate;
  obj = [self sanitize:obj];
  [self mutate_then_consume:obj];
}

- (void)test_simple_inheritance {
  InheritsNSObject* start = [InheritsNSObject new];
  [self consume:start];
}

@end
