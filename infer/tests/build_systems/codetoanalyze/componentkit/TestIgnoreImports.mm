/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "HeaderWithMutableLocalVar.h"

#include "../../../codetoanalyze/objcpp/linters/componentkit/FakeComponentKitHeader.h"

#include <vector>

struct D {};

@interface SomeClass : CKCompositeComponent
@end
@implementation SomeClass
+ (instancetype)new {
  int i; // error

  for (int i = 0; i < 10; i++) { // no error
  }

  std::vector<D*> v; // no error

  __block D* var; // no error
  return nil;
}
@end
