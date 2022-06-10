/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface DataFlowToSink : NSObject
@end

@implementation DataFlowToSink

- (NSObject*)might_be_a_source {
  return [NSObject new];
}

- (void)__infer_taint_sink:(NSObject*)obj {
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

- (void)test {
  NSObject* start = self.create_then_mutate;
  [self mutate_then_consume:start];
}

@end
