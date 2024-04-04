/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

@interface Singleton : NSObject

@property int x;

@end

typedef void (^MyBlock)();

void dispatch(MyBlock block) { block(); }

@implementation Singleton

- (int)dispatch_once_no_npe_good {
  static Singleton* a = nil;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    a = [[Singleton alloc] init];
  });
  return a->_x;
}

- (int)dispatch_once_captured_vars_bad {
  static dispatch_once_t onceToken;
  int* x = NULL;
  int val = 5;
  __block int* y = &val;
  dispatch_once(&onceToken, ^{
    y = x;
  });
  return *y;
}

- (int)dispatch_no_npe_good {
  static Singleton* a = nil;
  static dispatch_once_t onceToken;
  dispatch(^{
    a = [[Singleton alloc] init];
    a->_x = 5;
  });
  return a->_x;
}

@end

int captured_npe_bad() {
  int* x = NULL;
  int (^my_block)(void) = ^() {
    return *x;
  };
  return my_block();
}

int captured_npe_ok(int* y) {
  __block int* x = NULL;
  void (^my_block)(void) = ^() {
    x = y;
  };
  my_block();
  return *x;
}

void dispatch_sync_specializable(dispatch_queue_t queue,
                                 void(NS_NOESCAPE ^ f)(void)) {
  if (!f) {
    return;
  }
  if (queue) {
    dispatch_sync(queue, f);
  } else {
    f();
  }
}

int dispatch_sync_specialized_ok_FP(dispatch_queue_t queue) {
  __block int x = 0;
  __block int* ptr = NULL;
  dispatch_sync_specializable(queue, ^{
    ptr = &x;
  });
  return *ptr; // We get NULLPTR_DEREFERENCE_LATENT here because even though we
               // run the block, we also take into account other specs of
               // dispatch_sync_specializable without block execution and thus
               // we report NPE here.
}

int dispatch_sync_specialized_latent(dispatch_queue_t queue) {
  __block int x = 0;
  __block int* ptr = NULL;
  dispatch_sync_specializable(queue, ^{
    ptr = NULL;
  });
  return *ptr;
}

void performAsCurrentTraitCollection_specializable(
    UITraitCollection* traitCollection, void(NS_NOESCAPE ^ f)(void)) {
  if (!f) {
    return;
  }
  if (traitCollection) {
    [traitCollection performAsCurrentTraitCollection:f];
  } else {
    f();
  }
}

int performAsCurrentTraitCollection_specialized_ok_FP(
    UITraitCollection* traitCollection) {
  __block int x = 0;
  __block int* ptr = NULL;
  performAsCurrentTraitCollection_specializable(traitCollection, ^{
    ptr = &x;
  });
  return *ptr; // We get NULLPTR_DEREFERENCE_LATENT here because even though we
               // run the block, we also take into account other specs of
               // performAsCurrentTraitCollection_specializable without block
               // execution and thus we report NPE here.
}

int performAsCurrentTraitCollection_specialized_latent(
    UITraitCollection* traitCollection) {
  __block int x = 0;
  __block int* ptr = NULL;
  performAsCurrentTraitCollection_specializable(traitCollection, ^{
    ptr = NULL;
  });
  return *ptr;
}

void call_block(MyBlock block) { dispatch(block); }

void deep_npe_bad(int a) {
  __block int* ptr = NULL;
  call_block(^{
    *ptr = 0;
  });
}

void block_is_not_nil_ok() {
  MyBlock b = ^{
  };
  MyBlock c = [b copy];
  if (!b) {
    int* x = NULL;
    *x = 42;
  }
}

void block_is_not_nil_bad() {
  MyBlock b = ^{
  };
  MyBlock c = [b copy];
  if (b) {
    int* x = NULL;
    *x = 42;
  }
}
