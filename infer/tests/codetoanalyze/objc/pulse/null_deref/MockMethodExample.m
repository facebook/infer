/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

#define MOCK_DEF(context, api) \
  __typeof(__typeof(api)*) mockptr_##context##api = &api;
#define MOCK_USE(context, api) (*mockptr_##context##api)

int handleIntentForKey(int* p) { return *p; }

MOCK_DEF(MockMethodExample, handleIntentForKey);
#define handleIntentForKey MOCK_USE(MockMethodExample, handleIntentForKey)

int mock_ref_bad() { return handleIntentForKey(NULL); }
