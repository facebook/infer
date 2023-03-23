/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.


#import <Foundation/Foundation.h>

@protocol Provider<NSObject>

- (id)get;

@end

@interface Provider<__covariant T> : NSObject<Provider>

- (instancetype)initWithProvider:(id<Provider>)provider;

- (T)get;

@end

@implementation Provider {
  id<Provider> _provider;
}

- (instancetype)initWithProvider:(id<Provider>)provider {
  if (self = [super init]) {
    _provider = provider;
  }
  return self;
}

// we don't loop in the recursive call, so we get the NPE
- (id)get_bad {
  id item = [_provider get];
  int* p = NULL;
  int i = *p;
}

@end
