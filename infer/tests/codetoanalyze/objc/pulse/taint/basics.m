/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@class InferTaint;

typedef void (^InferTaintBlock)(InferTaint*);

@interface InferTaint : NSObject

@property(nonatomic, assign) NSObject* obj;

+ (NSObject*)source;
+ (void)taintsArg:(NSObject*)param;
+ (void)sink:(NSObject*)param;
+ (NSObject*)sanitizer:(NSObject*)param;
+ (void)sanitizeThenSink:(NSObject*)param;
+ (NSObject*)sanitizeThenTaint:(NSObject*)param;
+ (void)twoSinks:(NSObject*)param;
+ (void)twoKindSink:(NSObject*)param;
+ (void)notASink:(NSObject*)param;
+ (void)call_block:(InferTaintBlock)completion;
@end

@implementation InferTaint

+ (NSObject*)source {
  InferTaint* result = [InferTaint new];
  result.obj = [NSObject new];
  return result;
};

+ (void)taintsArg:(NSObject*)param {
}

+ (void)sink:(NSObject*)param {
}

+ (NSObject*)sanitizer:(NSObject*)param {
  return param;
}

+ (void)sanitizeThenSink:(NSObject*)param {
  NSObject* sanitized = [InferTaint sanitizer:param];
  [InferTaint sink:sanitized];
}

+ (NSObject*)sanitizeThenTaint:(NSObject*)param {
  NSObject* sanitized = [InferTaint sanitizer:param];
  [InferTaint taintsArg:sanitized];
  return sanitized;
}

+ (void)twoSinks:(NSObject*)param {
  [InferTaint sink:param];
  [InferTaint twoKindSink:param];
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

void callTwoKindSinkOnTwiceTaintedDirectBad() {
  NSObject* source = [InferTaint source];
  [InferTaint taintsArg:source];
  [InferTaint twoKindSink:source];
}

void callTwoSinksIndirectBad() {
  NSObject* source = [InferTaint source];
  [InferTaint twoSinks:source];
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

void sanitizerThenTaintDirectBad() {
  NSObject* obj = [NSObject new];
  obj = [InferTaint sanitizer:obj];
  [InferTaint taintsArg:obj];
  [InferTaint twoKindSink:obj];
}

void sanitizerThenTaintInterprocBad() {
  NSObject* obj = [NSObject new];
  [InferTaint sanitizeThenTaint:obj];
  [InferTaint twoKindSink:obj];
}

void fieldAccessBad() {
  InferTaint* source = [InferTaint source];
  NSObject* obj = source.obj;
  [InferTaint sink:obj];
}

void compoundStmtBad() {
  InferTaint* taint = ({
    assert(true);
    InferTaint.source;
  });
  [InferTaint sink:taint];
}

void compoundStmt_taintSourceParameterBad(InferTaint* source) {
  InferTaint* taint = ({
    assert(true);
    source;
  });
  [InferTaint sink:taint];
}

void clashingBlockNameOk(NSObject* tainted) {
  ^void(NSObject* obj) {
    [InferTaint sink:obj];
  };
}

void taintSourceParameterBlockIndirect(InferTaintBlock completion) {
  [InferTaint call_block:^(InferTaint* source) {
    completion(source);
  }];
}

void taintSourceParameterBlockIndirectBad_FN() {
  taintSourceParameterBlockIndirect(^(InferTaint* arg) {
    [InferTaint sink:arg];
  });
}
