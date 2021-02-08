/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface CallBlocks : NSObject

- (void)take_two_blocks:(NSInteger*)n
                 block1:(void (^)(NSInteger*))block1
                 block2:(void (^)(NSInteger*))block2;

@end

@implementation CallBlocks

- (id)init {
  return self;
}

- (void)take_two_blocks:(NSInteger*)n
                 block1:(void (^)(NSInteger*))block1
                 block2:(void (^)(NSInteger*))block2 {
  block1(n);
  block2(n);
}

@end

@interface Handler : NSObject
@end

@implementation Handler

- (void (^)(NSString*))func_linear {
  return ^(NSString* str) {
    NSLog(@"Report error : %@ \n", str);
    for (int i = 0; i < str.length; i++) {
    } // we get a complexity increase here for the block, but we should print it
      // better
  };
}

+ (void)loop_linear:(NSArray*)array {
  for (int i = 0; i < array.count; i++) {
  }
}

- (void)func_linear:(NSArray*)array {
  for (id value in array) {
    [Handler loop_linear:array];
  }
}

- (void)create_block_here:(NSInteger*)n
                    array:(NSArray*)array
                     call:(CallBlocks*)call {
  [call take_two_blocks:n
      block1:^(NSInteger* n) {
        for (id value in array) {
          n += [value integerValue];
        }
      }
      block2:^(NSInteger* n) {
        NSLog(@"Report error");
      }];
}
@end

int main() { return 0; }
