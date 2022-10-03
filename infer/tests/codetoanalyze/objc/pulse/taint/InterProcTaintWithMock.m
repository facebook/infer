/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

#define MOCK_DEF(context, api) \
  __typeof(__typeof(api)*) mockptr_##context##api = &api;
#define MOCK_USE(context, api) (*mockptr_##context##api)

void HandleIntent(NSString* key, NSString* context, NSObject* input) {}

void dispatchedWithMock(NSString* context, NSObject* input) {}

extern NSString* StringProviderInputCreate() { return @"input"; }

static NSString* AttributedStringProviderInput() {
  return StringProviderInputCreate();
}

MOCK_DEF(InterProcTaintWithMock, HandleIntent);
#define HandleIntent MOCK_USE(InterProcTaintWithMock, HandleIntent)

NSString* taintInterprocWithMockBad() {

  NSArray<NSString*>* const matches = @[ @"1", @"2" ];

  for (NSString* textResult in matches) {
    HandleIntent(
        @"dispatch_1", @"context", (NSObject*)StringProviderInputCreate());
  };

  return @"string";
}
