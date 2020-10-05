/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

typedef int (^GetNum)();

@interface MyObject : NSObject
@end

@implementation MyObject {
  GetNum _get_num;
}

- (void)set_get_num:(GetNum)get_num {
  _get_num = get_num;
}

- (GetNum)get_get_num {
  return _get_num;
}

@end

@interface Block : NSObject
@end

@implementation Block

- (void)block_in_field_Good {
  int a[10];
  MyObject* o = [MyObject new];
  [o set_get_num:^int {
    return 5;
  }];
  GetNum get_num = [o get_get_num];
  a[get_num()] = 0;
}

- (void)block_in_field_Bad {
  int a[10];
  MyObject* o = [MyObject new];
  [o set_get_num:^int {
    return 15;
  }];
  GetNum get_num = [o get_get_num];
  a[get_num()] = 0;
}

@end
