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
+ (NSObject*)sanitizer:(NSObject*)param;
+ (void)sanitizeThenSink:(NSObject*)param;
+ (void)twoKindSink:(NSObject*)param;
+ (void)notASink:(NSObject*)param;

+ (void)call_block:(void (^)(InferTaint*))completion;
@end

@implementation InferTaint

+ (NSObject*)source {
  return [NSObject new];
};

+ (void)sink:(NSObject*)param {
}

+ (NSObject*)sanitizer:(NSObject*)param {
  return param;
}

+ (void)sanitizeThenSink:(NSObject*)param {
  NSObject* sanitized = [InferTaint sanitizer:param];
  [InferTaint sink:sanitized];
}

+ (void)twoKindSink:(NSObject*)param {
}

+ (void)notASink:(NSObject*)param {
}

@end

void callSinkDirectBad() {
  NSObject* source = [InferTaint source];
  [InferTaint sink:source];
}

void callTwoKindSinkDirectBad() {
  NSObject* source = [InferTaint source];
  [InferTaint twoKindSink:source];
}

void callSinkOnNonSourceOk() {
  NSObject* source = [NSObject new];
  [InferTaint sink:source];
}

void callNonSinkOnSourceOk() {
  NSObject* source = [InferTaint source];
  [InferTaint notASink:source];
}

void taintSourceParameterBad(InferTaint* source) { [InferTaint sink:source]; }

void taintSourceParameterBlockBad() {
  [InferTaint call_block:^(InferTaint* source) {
    [InferTaint sink:source];
  }];
}

void viaSanitizerOk() {
  NSObject* source = [InferTaint source];
  [InferTaint sanitizeThenSink:source];
}
