/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

#define MOCK_DEF(context, api)
#define MOCK_USE(context, api) api

int handleKey(int* p) { return *p; }

MOCK_DEF(NoMockMethodExample, handleKey);
#define handleKey MOCK_USE(NoMockMethodExample, handleKey)

int no_mock_ref_bad() { return handleKey(NULL); }
