/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface EscapingByref : NSObject
@end

@implementation EscapingByref

int global;

+ (void)capture_global_byref {
  void (^closure)() = ^() {
    global = 3;
  };
}

+ (void)capture_local_byvalue {
  int local;
  void (^closure)() = ^() {
    int x = local;
  };
}

+ (void)capture_local_block_byref {
  __block int local;
  void (^closure)() = ^() {
    local = 3;
  };
}

+ (void)capture_local_static_byvalue {
  static int local = 3;
  void (^closure)() = ^() {
    int x = local;
  };
}

+ (void)capture_param_byvalue:(int)param {
  void (^closure)() = ^() {
    int x = param;
  };
}

@end
