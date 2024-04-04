/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

int conditionalBasicOK() {
  int x = 5;
  x = x ? x : 1;
  return x;
}

int conditionalOpaqueOk() {
  int x = 5;
  x = x ?: 1;
  return x;
}

NSURLRequest* const getRequest();

NSURL* const conditionalOpaque2Ok(NSURL* x) {
  NSURLRequest* const request = getRequest();
  return x ?: request.URL;
}

Class messageExprSpecialCaseOk() {
  NSURLRequest* const x = getRequest();
  Class c = [(x ? x : x) class];
  return c;
}
