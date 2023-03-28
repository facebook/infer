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
+ (void)sink:(InferTaint*)param;
+ (void)callBlockUnknown:(InferTaintBlock)completion;
@end

void taintSourceParameterBlockBad() {
  [InferTaint callBlockUnknown:^(InferTaint* source) {
    [InferTaint sink:source];
  }];
}
