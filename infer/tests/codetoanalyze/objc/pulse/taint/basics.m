/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface InferTaint : NSObject

+ (NSObject*)source;
+ (void)sink:(NSObject*)param;
+ (void)notASink:(NSObject*)param;
@end

@implementation InferTaint

+ (NSObject*)source {
  return [NSObject new];
};

+ (void)sink:(NSObject*)param {
}

+ (void)notASink:(NSObject*)param {
}

@end

void callSinkDirectBad() {
  NSObject* source = [InferTaint source];
  [InferTaint sink:source];
}

void callSinkOnNonSourceOk() {
  NSObject* source = [NSObject new];
  [InferTaint sink:source];
}

void callNonSinkOnSourceOk() {
  NSObject* source = [InferTaint source];
  [InferTaint notASink:source];
}
