/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include "../../../../codetoanalyze/objcpp/linters/componentkit/FakeComponentKitHeader.h"

@interface SomeClass : CKCompositeComponent
@end
@implementation SomeClass
+ (instancetype)new {
  int i; // error
  return nil;
}
@end
