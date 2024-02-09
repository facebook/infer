/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

typedef void (^MyHandler)(NSString* name);

@interface SharedView : NSObject

@property(nonatomic, strong) MyHandler handler;
@property(nonatomic, strong) NSString* name;
@property(nonatomic, strong) SharedView* view;

@end

@implementation SharedView

- (void)sharedInstanceRetainCycle_bad {
  static dispatch_once_t once;
  static SharedView* sharedInstance;
  dispatch_once(&once, ^{
    sharedInstance = [[SharedView alloc] init];
  });

  _view = sharedInstance;
  sharedInstance.handler = ^(NSString* s) {
    _name = sharedInstance.name;
  };
}

@end

int test_sharedInstanceRetainCycleBad() {
  SharedView* a = [SharedView new];
  [a sharedInstanceRetainCycle_bad];
  SharedView* b = [SharedView new];
  [b sharedInstanceRetainCycle_bad];
  return 0;
}
