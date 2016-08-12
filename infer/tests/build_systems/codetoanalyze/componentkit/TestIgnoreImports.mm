/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include "HeaderWithMutableLocalVar.h"

#define nil 0

// Mimic importing CKComponnet
@interface CKComponent
@end
@implementation CKComponent
@end

// Mimic importing CKCompositeComponnet
@interface CKCompositeComponent : CKComponent
+ (instancetype)newWithComponent:(CKComponent*)component;
@end
@implementation CKCompositeComponent
+ (instancetype)newWithComponent:(CKComponent*)component {
  return nil;
}
@end

@interface SomeClass : CKCompositeComponent
@end
@implementation SomeClass
+ (instancetype) new {
  int i; // error
  return nil;
}
@end
