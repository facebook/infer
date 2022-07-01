/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

extern NSString* ProviderInputCreate() { return @"input"; }

static NSString* ProviderInput() { return ProviderInputCreate(); }

const void* _Nullable VaultOpenAppleMapForAddress(NSObject* input);

const void* _Nullable AddressActionTapIntentHandler_HandleIntent(
    NSString* context, NSObject* input) {
  VaultOpenAppleMapForAddress(input);
}

static const void* _Nullable _LoadExtensionFunction(int key,
                                                    NSString* context,
                                                    NSObject* input) {
  if (key == 2) {
    return AddressActionTapIntentHandler_HandleIntent(context, input);
  } else {
    return NULL;
  }
}

void HandlerForKey(int key, NSString* context, NSObject* input) {
  void (*func)(id, NSObject*) =
      (void (*)(NSObject*))_LoadExtensionFunction(key, context, input);

  if (func != NULL) {
    func(context, input);
  }
}

NSString* taintFuncPointerBad() {

  NSArray<NSString*>* const matches = @[ @"1", @"2" ];

  for (NSString* textResult in matches) {
    HandlerForKey(@"key", @"context", (NSObject*)ProviderInput());
  };

  return @"string";
}
