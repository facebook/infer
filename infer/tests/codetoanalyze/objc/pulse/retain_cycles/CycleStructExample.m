/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@class BridgingDelegate;

struct Person {
  int _age;
  BridgingDelegate* _responderBridgingDelegate;
};

@interface BridgingDelegate : NSObject
@end

@implementation BridgingDelegate {
  struct Person* _person;
}

+ (instancetype)newWithPerson:(struct Person*)person {
  BridgingDelegate* s = [self new];
  if (s) {
    s->_person = person;
  }
  return s;
}

@end

void test_no_retain_cycle_good() {
  struct Person* person = (struct Person*)malloc(sizeof(struct Person));
  BridgingDelegate* responderBridgingDelegate =
      [BridgingDelegate newWithPerson:person];
  person->_responderBridgingDelegate = responderBridgingDelegate;
  free(person);
}
