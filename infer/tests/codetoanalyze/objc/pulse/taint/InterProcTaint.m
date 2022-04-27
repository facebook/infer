/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

void HandleIntentForKey(NSString* key, NSString* context, NSObject* input) {}

extern NSString* AttributedStringProviderInputCreate() { return @"input"; }

static NSString* AttributedStringProviderInput() {
  return AttributedStringProviderInputCreate();
}

NSString* taintInterprocBad() {

  NSArray<NSString*>* const matches = @[ @"1", @"2" ];

  for (NSString* textResult in matches) {
    HandleIntentForKey(
        @"key", @"context", (NSObject*)AttributedStringProviderInput());
  };

  return @"string";
}
