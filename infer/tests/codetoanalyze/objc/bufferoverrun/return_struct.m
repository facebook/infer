/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

struct s {
  int a;
};

@interface ReturnStruct : NSObject
@end

@implementation ReturnStruct

- (struct s)get_struct:(int)n {
  struct s x;
  x.a = n;
  return x;
}

- (void)call_get_struct_Ok {
  int arr[5];
  struct s x = [self get_struct:3];
  arr[x.a] = 0;
}

- (void)call_get_struct_Bad {
  int arr[3];
  struct s x = [self get_struct:5];
  arr[x.a] = 0;
}

@end
