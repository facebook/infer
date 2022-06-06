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

NSObject* might_be_a_source() { return [NSObject new]; }

void __infer_taint_sink(NSObject* obj) {}

NSObject* create_taint() { return might_be_a_source(); }

NSObject* identity(NSObject* obj) { return obj; }

NSObject* create_then_mutate() {
  NSObject* source = create_taint();
  source = identity(source);
  return source;
}

void consume(NSObject* obj) { __infer_taint_sink(obj); }

void mutate_then_consume(NSObject* obj) {
  obj = identity(obj);
  consume(obj);
}

void test() {
  NSObject* start = create_then_mutate();
  mutate_then_consume(start);
}

@end
