/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

struct Origin {
  int x;
  int y;
};

struct Rectangle {
  int width;
  int height;
  NSString* title; // bug
  Origin origin;
};

@interface SomeButton : NSObject

+ (instancetype)newWithFrame:(Rectangle)aRect;

@end

SomeButton* buttonComponent(void);
SomeButton* buttonComponent(void) {
  SomeButton* c;
  // bug
  c = [SomeButton newWithFrame:{}];
  // bug
  c = [SomeButton newWithFrame:{.width = 100, .height = 200}];
  // no bug
  c = [SomeButton newWithFrame:{.title = @"Button"}];
  // bug
  c = [SomeButton newWithFrame:{.origin = {.x = 20, .y = 20}}];

  return c;
};
