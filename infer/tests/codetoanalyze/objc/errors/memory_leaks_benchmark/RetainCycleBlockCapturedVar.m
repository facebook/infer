/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/NSObject.h>

@interface Listener : NSObject

@property(nonatomic, copy) void (^didFinishLoad)();

@end

@implementation Listener

- (void)dealloc {
  NSLog(@"dealloc Listener");
}

@end

@interface LinkResolver : NSObject

@end

@implementation LinkResolver

- (void)test_bad {
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
  [a test_bad];
  return 0;
}
