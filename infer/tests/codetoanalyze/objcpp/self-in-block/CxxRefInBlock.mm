/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#include <stdlib.h>
#include <vector>
#import <Foundation/Foundation.h>

typedef void (^MyHandler)();

struct A {
  template <typename Model>
  void call_block(std::vector<Model> models, NS_NOESCAPE MyHandler block) {}
};

typedef struct InternalInput {
  BOOL flag;
  int x;
} InternalInput;

@interface Task : NSObject
@property(nonatomic) int result;
@end

@interface CxxRefInBlock : NSObject
- (int)foo:(int&)y;
@end

@class Attachments;

void foo(Attachments* attachments);

@implementation CxxRefInBlock

- (int)ref_captured_in_escaping_block_bad:(int&)y and:(int*)ptr {
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = y;
    int i = *ptr;
    return;
  });
  return 1;
}

- (int)ref_captured_in_escaping_block_bad:(int*)ptr {
  __block int& y = *ptr;
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = y;
    int i = *ptr;
    return;
  });
  return 1;
}

- (int)ref_captured_struct_good {
  __block InternalInput internalInput = {
      .flag = NO,
      .x = 0,
  };
  dispatch_async(dispatch_get_main_queue(), ^{
    internalInput.x = 5;
  });
  return internalInput.x;
}

- (int)ref_captured_class_good {
  __block Task* task = [Task new];
  dispatch_async(dispatch_get_main_queue(), ^{
    task.result = 5;
  });
  return task.result;
}

- (int)ref_captured_int_good {
  __block uint64_t startTimestamp = 0;
  __block int32_t threadIdx = 0;
  dispatch_async(dispatch_get_main_queue(), ^{
    startTimestamp = 5;
    threadIdx = 5;
  });
  return startTimestamp;
}

- (int)ref_captured_forward_class_good {
  __block Attachments* set;
  dispatch_async(dispatch_get_main_queue(), ^{
    foo(set);
  });
  return 0;
}

- (int)ref_captured_bool_good {
  auto __block hasMutated = NO;
  dispatch_async(dispatch_get_main_queue(), ^{
    hasMutated = YES;
  });
  return 0;
}

- (int)ref_captured_in_no_escaping_block_good:(int&)y {
  dispatch_sync(dispatch_get_main_queue(), ^{
    int a = y;
    return;
  });
  return 1;
}

- (int)multiple_refs_captured_in_escaping_block_bad:(int&)y param2:(int&)z {
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = y;
    int b = z;
    return;
  });
  return 1;
}

- (int)ref_copied_good:(int&)y {
  const int copied_y = y;
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = copied_y;
    return;
  });
  return 1;
}

- (int)ref_captured_in_noescaping_block_good:(int&)y and:(int*)ptr {
  std::vector<int> v;
  A* a = new A();
  a->call_block(v, ^{
    int a = y;
    int i = *ptr;
  });
  return 1;
}

- (int)ref_captured_in_local_block_good:(int&)y and:(int*)ptr {
  MyHandler block = ^{
    int a = y;
    int i = *ptr;
  };
  block();
  return 1;
}

@end
