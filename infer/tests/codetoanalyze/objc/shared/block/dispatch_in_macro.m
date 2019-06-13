/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

#define INITIALIZE_AND_RETURN_STATIC(...)          \
  ({                                               \
    static __typeof__(__VA_ARGS__) static_storage; \
    static dispatch_once_t once_token;             \
    dispatch_once(&once_token, ^{                  \
      static_storage = (__VA_ARGS__);              \
    });                                            \
    static_storage;                                \
  })

id DispatchInMacroTest() {
  return INITIALIZE_AND_RETURN_STATIC([NSObject new]);
}
