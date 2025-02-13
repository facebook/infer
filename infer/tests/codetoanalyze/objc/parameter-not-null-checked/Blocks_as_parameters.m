/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

typedef void (^NewAnnotateBlock1)(const char* _Nullable key,
                                  id _Nullable value);
typedef void (^_Nullable NewAnnotateSyncBlock1)(NewAnnotateBlock1 annotate);

void MarkerAnnotateSync1(NS_NOESCAPE _Nullable NewAnnotateSyncBlock1 block) {}

void testAnnotateOk(NSString* composerSessionID) {
  MarkerAnnotateSync1(^(NewAnnotateBlock1 annotate) {
    annotate("composer_session_id", composerSessionID);
  });
}

typedef void (^MyBlock)();

typedef void (^MyBlock1)(int x);

@interface Blocks_as_parameters : NSObject

@end

@implementation Blocks_as_parameters

+ (void)blockNotCheckedBad:(int)z and:(MyBlock)block {
  block();
}

+ (void)blockNotCheckedBad1:(int)z and:(MyBlock1)block {
  block(z);
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

- (void)uploadTaskWithRequestWithCheckOk:(NSURLRequest*)urlRequest
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


- (void)uploadTaskWithRequestBad:(NSURLRequest*)urlRequest
                        fromFile:(NSURL*)fileURL
                        delegate:(id)delegate
                   delegateQueue:(NSOperationQueue*)delegateQueue
                      completion:(MyBlock)completion {
  [Blocks_as_parameters addOperationWithBlock:^{
    completion();
  }];
}

- (void)uploadTaskWithRequestOk:(NSURLRequest*)urlRequest
                       fromFile:(NSURL*)fileURL
                       delegate:(id)delegate
                  delegateQueue:(NSOperationQueue*)delegateQueue
                     completion:(_Nonnull MyBlock)completion {
  [Blocks_as_parameters addOperationWithBlock:^{
    completion();
  }];
}

- (void)capturedNotFormalOK {
  MyBlock block;

  [Blocks_as_parameters addOperationWithBlock:^{
    block();
  }];
}

typedef void (^_Nonnull AnnotateBlock)(const char* key, id value);

typedef void (^AnnotateSyncBlock)(AnnotateBlock _Nonnull annotate);
typedef void (^AnnotateSyncBlock1)(AnnotateBlock annotate);

void AnnotateSync(id flowId, NS_NOESCAPE AnnotateSyncBlock block) {}
void AnnotateSync1(id flowId, NS_NOESCAPE AnnotateSyncBlock1 block) {}

+ (void)startWithSessionOk:(id)session video:(id)video {
  NSString* const videoID = @"";
  NSString* const flowId = @"";

  NSString* const entryPointType = @"";

  AnnotateSync(flowId, ^(AnnotateBlock _Nonnull annotate) {
    annotate("entry_point_type", entryPointType);
    annotate("media_id", videoID);
  });
}

+ (void)startWithSessionOk:(NSString*)composerSessionID {
  NSString* const flowId = @"";
  AnnotateSync1(flowId, ^(AnnotateBlock annotate) {
    annotate("entry_point_type", composerSessionID);
  });
}

+ (void)startWithSession2Ok:(id)session video:(id)video {
  NSString* const videoID = @"";
  NSString* const flowId = @"";

  NSString* const entryPointType = @"";

  AnnotateSync(flowId, ^(AnnotateBlock annotate) {
    annotate("entry_point_type", entryPointType);
    annotate("media_id", videoID);
  });
}

#ifndef BLOCK_CALL_SAFE_ON_QUEUE
#define BLOCK_CALL_SAFE_ON_QUEUE(QUEUE, BLOCK, ...) \
  ((BLOCK) ? dispatch_async(QUEUE,                  \
                            ^{                      \
                              (BLOCK)(__VA_ARGS__); \
                            })                      \
           : (void)0)
#endif

+ (void)block_call_safe_on_queue_macro_ok:(MyBlock)completion {
  BLOCK_CALL_SAFE_ON_QUEUE(dispatch_get_main_queue(), completion, nil);
}

- (void)startWithCompletionHandlerOk:(MyBlock)completion {
  if (completion != nil) {
    dispatch_async(dispatch_get_main_queue(), ^{
      completion();
    });
  }
}

@end

#ifdef __clang__
#pragma clang assume_nonnull begin
#endif

typedef void (^NewAnnotateBlock)(const char* _Nullable key, id _Nullable value);
typedef void (^_Nullable NewAnnotateSyncBlock)(
    _Nonnull NewAnnotateBlock annotate);

#ifdef __clang__
#pragma clang assume_nonnull end
#endif

void MarkerAnnotateSync(NS_NOESCAPE _Nullable NewAnnotateSyncBlock block) {}

void LoadAndAnnotateOk(NSString* composerSessionID) {
  MarkerAnnotateSync(^(NewAnnotateBlock annotate) {
    annotate("composer_session_id", composerSessionID);
  });
}

typedef int (^_Nonnull AnotherBlock)();

int blockNotCheckedBadNoAutofix(AnotherBlock block) { return block(); }
