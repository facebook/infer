/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

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

@interface Session
- (NSArray<NSObject*>*)getSource;
@end

@implementation Session
- (NSArray<NSObject*>*)getSource {
  NSArray* ret = @[];
  return ret;
}

@end

@interface Helper : NSObject
- (BOOL)b1;
- (BOOL)b2;
@end

@implementation Helper {
  Session* _session;
  Helper* _helper;
}

- (BOOL)b1 {
  return true;
}

- (BOOL)b2 {
  return false;
}

- (BOOL)_isPositive {
  return [[_session getSource] count] > 0;
}

- (void)taints_unrelated_field_ok_FP {
  if ([self _isPositive]) {
  } // in the state we have v, the result of [self _isPositive], which is
    // tainted and equal to 0
  if ([_helper b1]) { // nil spec is applied to b1, hence _helper becomes 0,
                      // equal to v, and hence tainted
  }
  BOOL b = [_helper b2]; // sensitive data flow FP is reported because in
                         // previous step _helper became tainted
}
@end
