/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface CategoryPerson : NSObject
@property int age;

- (instancetype)initWithAge:(int)age;

@end

@interface CategoryPerson (Child)
- (bool)isChild;
@end

@implementation CategoryPerson

- (instancetype)initWithAge:(int)age {
  _age = age;
  return self;
}

@end

@implementation CategoryPerson (Child)

- (bool)isChild {
  if (_age < 18) {
    return true;
  } else {
    return false;
  }
}
@end

int categoryMethodsOkIsChildNPEBad() {
  CategoryPerson* person = [[CategoryPerson alloc] initWithAge:10];
  if ([person isChild]) {
    int* p = NULL;
    *p = 42;
  }
}

int isNotChildNoNPEGood() {
  CategoryPerson* person = [[CategoryPerson alloc] initWithAge:25];
  if ([person isChild]) {
    int* p = NULL;
    *p = 42;
  }
}
