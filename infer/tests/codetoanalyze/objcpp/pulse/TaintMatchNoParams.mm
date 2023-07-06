/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@class InferTaint;

typedef void (^InferTaintBlock)(NSObject*);

@interface InferTaint : NSObject

+ (NSObject*)source;

@end

@implementation InferTaint

+ (NSObject*)source {
  return [InferTaint new];
}

@end

@interface Data
@end

void call_block(Data* data, InferTaintBlock completion) {
  NSObject* source = [InferTaint source];
  completion(source);
}

static void block_passed_to_match_good(Data* data) {
  call_block(data, ^(NSObject* source) {
    NSLog(source);
  });
}

void DataPassing(InferTaintBlock completion) {
  NSObject* source = [InferTaint source];
  completion(source);
}

static void block_passed_to_match_bad(Data* data) {
  DataPassing(^(NSObject* source) {
    NSLog(source);
  });
}
