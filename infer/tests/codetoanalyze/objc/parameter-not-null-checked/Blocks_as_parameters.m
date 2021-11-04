/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

typedef void (^MyBlock)();

@interface Blocks_as_parameters : NSObject

@end

@implementation Blocks_as_parameters

+ (void)blockNotCheckedBad:(int)z and:(MyBlock)block {
  block();
}
+ (void)blockCheckedOk:(int)z and:(MyBlock)block {
  if (block) {
    block();
  }
  if (block != nil) {
    block();
  }
  if (!(block == nil)) {
    block();
  }
}

+ (void)twoBlocksNotCheckedBad:(int)z and:(MyBlock)block1 and:(MyBlock)block2 {
  if (block1 || block2) {
    block1();
    block2();
  }
}

+ (void)twoBlocksCheckedOk:(int)z and:(MyBlock)block1 and:(MyBlock)block2 {
  if (block1 && block2) {
    block1();
    block2();
  }
}

+ (void)blockCheckedForNilOk:(int)z and:(MyBlock)block {
  if (nil != block) {
    block();
  }
  if (!(nil == block)) {
    block();
  }
}

+ (void)nonnullBlockOk:(int)z and:(_Nonnull MyBlock)block {
  block();
}

- (void)nonnullBlockTwoBlocksBad:(int)z
                             and:(_Nonnull MyBlock)block1
                             and:(MyBlock)block2 {
  block1();
  block2();
}

@end
