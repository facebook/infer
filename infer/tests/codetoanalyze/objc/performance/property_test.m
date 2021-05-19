/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import "MyData.h"

void getter_property_linear(MyData* data) {
  NSString* dn = data.name;
  for (int i = 0; i < dn.length; i++) {
  }
}
