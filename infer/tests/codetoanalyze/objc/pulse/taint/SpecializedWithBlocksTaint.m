/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <Foundation/Foundation.h>

@interface Builder : NSObject

@property(nonatomic, strong) NSString* value;

@end

typedef void (^fetchAndRunHandlerBlock)(NSString* account);

@interface AccessLibrary : NSObject

- (NSString*)fetchResult;

- (void)fetchWithCompletion:(fetchAndRunHandlerBlock)completion;

@end

@implementation AccessLibrary

- (void)fetchWithCompletion:(fetchAndRunHandlerBlock)completion {
  NSString* result = [self fetchResult];
  completion(result);
}

static void fetchAndRunHandler(fetchAndRunHandlerBlock completionBlock) {
  AccessLibrary* library = [AccessLibrary new];
  [library fetchWithCompletion:^(NSString* account) {
    completionBlock(account);
  }];
}

@end

void callSpecializedWithBlocksBad() {
  Builder* builder = [Builder new];
  void (^completionBlock)(NSString* account) = ^(NSString* account) {
    [builder setValue:account];
  };
  fetchAndRunHandler(completionBlock);
}
