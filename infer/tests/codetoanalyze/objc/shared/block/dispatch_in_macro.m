/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
