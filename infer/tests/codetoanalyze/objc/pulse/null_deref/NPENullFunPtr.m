/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

void void_id(void) { return; }

@interface FunPtrA : NSObject
@end

@implementation FunPtrA {

  void (*_funptr_field)(void);
}

- (void)doSomethingThenCallbackFunPtr:(void (*)(void))my_funptr {
  // null dereference, segfault if my_funptr is NULL
  (*my_funptr)();
}

- (void)assignNilBad {

  void (*my_funptr)(void) = &void_id;
  my_funptr = NULL;
  my_funptr(); // Null deref
}

- (void)checkNotNilOk:(void (*)(void (*)(void)))my_funptr
         funptr_param:(void (*)(void))funptr_param {
  // ok to call this funptr!
  if (my_funptr != NULL) {
    (*my_funptr)(funptr_param);
  }
}

- (void)paramAssignNilBad:(void (*)(void))my_funptr {

  my_funptr = NULL;
  (*my_funptr)(); // Null deref
}

- (void)paramReassignNilBad:(void (*)(void))my_funptr_param {

  void (*my_funptr)(void) = &void_id;
  my_funptr = NULL;
  my_funptr_param = my_funptr;
  (*my_funptr_param)(); // Null deref
}

- (void)assignEmptyFunPtrOk:(void (*)(void))my_funptr_param {

  void (*my_funptr)(void) = &void_id;
  my_funptr_param = my_funptr;
  (*my_funptr_param)(); // No error here
}

void call_funptr_param(void (*funptr_param)(void)) {
  if (funptr_param) {
    (*funptr_param)();
  }
}

void call_funptr(void (*funptr)(void (*)(void)), void (*funptr_param)(void)) {
  (*funptr)(funptr_param);
}

- (void)checkNotNilFunPtrAsArgOk:(BOOL)a
                    funptr_param:(void (*)(void))funptr_param {

  void (*my_funptr)(void (*)(void)) = &call_funptr_param;

  if (a) {
    [self checkNotNilOk:&call_funptr funptr_param:funptr_param];
  }
}

- (void)FN_ivarNilFunPtrBad {
  _funptr_field(); // Ivar not nullable
}

@end

void calldoSomethingThenCallbackFunPtrOk() {
  FunPtrA* funptrA = [FunPtrA alloc];
  void (*my_funptr)(void) = &void_id;
  [funptrA doSomethingThenCallbackFunPtr:my_funptr];
}

void calldoSomethingThenCallbackFunPtrWithNilBad() {
  FunPtrA* funptrA = [FunPtrA alloc];
  [funptrA doSomethingThenCallbackFunPtr:NULL];
}

void NULLFunPtrCallCFuntionBad() {
  void (*my_funptr)(void) = &void_id;
  my_funptr = NULL;
  my_funptr();
}
