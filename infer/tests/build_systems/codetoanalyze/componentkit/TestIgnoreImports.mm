/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include "HeaderWithMutableLocalVar.h"

#include "../../../codetoanalyze/objcpp/linters/componentkit/FakeComponentKitHeader.h"

#include <vector>

struct D {};

@interface SomeClass : CKCompositeComponent
@end
@implementation SomeClass
+ (instancetype) new {
  int i; // error

  for (int i = 0; i < 10; i++) { // no error
  }

  std::vector<D*> v; // no error

  __block D* var; // no error
  return nil;
}
@end
