/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface Listener : NSObject

@property(nonatomic, strong) void (^didFinishLoad)();

@end

@implementation Listener

- (void)dealloc {
  NSLog(@"dealloc Listener");
}

@end

@interface LinkResolver : NSObject

@end

@implementation LinkResolver

- (void)test {
  Listener* listener = [[Listener alloc] init];
  __block Listener* retainedListener = listener;
  listener.didFinishLoad = ^() {
    if (retainedListener) {
      retainedListener = nil;
    }
  };
}

- (void)dealloc {
  NSLog(@"dealloc LinkResolver");
}

@end

int main() {
  LinkResolver* a = [LinkResolver new];
  [a test];
  return 0;
}
