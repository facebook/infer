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

+ (void)call_block:(void (^)(InferTaint*))completion;
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

void callSinkOnNonSourceBad() {
  NSObject* source = [NSObject new];
  [InferTaint sink:source];
}

void callNonSinkOnSourceBad() {
  NSObject* source = [InferTaint source];
  [InferTaint notASink:source];
}

void taintSourceParameterBad(InferTaint* source) { [InferTaint sink:source]; }

void taintSourceParameterBlockBad() {
  [InferTaint call_block:^(InferTaint* source) {
    [InferTaint sink:source];
  }];
}
