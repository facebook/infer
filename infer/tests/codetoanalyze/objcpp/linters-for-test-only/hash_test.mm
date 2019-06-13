/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>
#import <functional>

template <>
struct std::hash<NSObject*> {
  size_t operator()(const NSObject* const& obj1) const { return [obj1 hash]; }
};
