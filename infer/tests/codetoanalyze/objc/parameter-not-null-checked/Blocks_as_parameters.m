/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

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

+ (void)blockCheckedAssignNULLBad:(int)z and:(MyBlock)block {
  if (block) {
    block = NULL;
    block();
  }
}

+ (void)blockAssignNonnullBlockOk:(int)z
                              and:(MyBlock)block
                              and:(_Nonnull MyBlock)block2 {
  block = block2;
  block();
}

+ (void)addOperationWithBlock:(void (^)())completion {
}

- (void)uploadTaskWithRequestOk:(NSURLRequest*)urlRequest
                       fromFile:(NSURL*)fileURL
                       delegate:(id)delegate
                  delegateQueue:(NSOperationQueue*)delegateQueue
                     completion:(void (^)())completion {
  if (!completion) {
    return;
  }

  [Blocks_as_parameters addOperationWithBlock:^{
    completion();
  }];
}

typedef void (^AnnotateBlock)(const char* key, id value);
typedef void (^AnnotateSyncBlock)(AnnotateBlock annotate);

void AnnotateSync(id flowId, NS_NOESCAPE AnnotateSyncBlock block) {}

+ (void)startWithSessionOk:(id)session video:(id)video {
  NSString* const videoID = @"";
  NSString* const flowId = @"";

  NSString* const entryPointType = @"";

  AnnotateSync(flowId, ^(AnnotateBlock _Nonnull annotate) {
    annotate("entry_point_type", entryPointType);
    annotate("media_id", videoID);
  });
}

+ (void)startWithSessionBad:(id)session video:(id)video {
  NSString* const videoID = @"";
  NSString* const flowId = @"";

  NSString* const entryPointType = @"";

  AnnotateSync(flowId, ^(AnnotateBlock annotate) {
    annotate("entry_point_type", entryPointType);
    annotate("media_id", videoID);
  });
}

@end
