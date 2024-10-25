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

void foo(std::vector<int> v) {}

std::string global_name = "global_name";

@interface CxxStringInBlock : NSObject
- (int)foo:(int&)y;
@end

@implementation CxxStringInBlock

- (void)local_string_captured_in_escaping_block_bad {
  std::string fullName;
  const char* c = fullName.c_str();
  dispatch_async(dispatch_get_main_queue(), ^{
    const char* c1 = c;
  });
}

- (void)local_string_captured_in_no_escaping_block_good {
  std::string fullName;
  const char* c = fullName.c_str();
  dispatch_sync(dispatch_get_main_queue(), ^{
    const char* c1 = c;
  });
}

- (void)formal_string_captured_in_escaping_block_good:(std::string)fullName {
  const char* c = fullName.c_str();
  dispatch_async(dispatch_get_main_queue(), ^{
    const char* c1 = c;
  });
}

- (void)local_vector_captured_in_escaping_block_good {
  std::vector<int> v;
  dispatch_async(dispatch_get_main_queue(), ^{
    foo(v);
  });
}

- (void)global_string_captured_in_escaping_block_good {
  const char* c = global_name.c_str();
  dispatch_async(dispatch_get_main_queue(), ^{
    const char* c1 = c;
  });
}
@end
