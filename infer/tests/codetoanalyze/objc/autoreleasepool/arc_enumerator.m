/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import "no_arc_callee.h"

typedef BOOL (^Filter)(id);

@interface MyEnumerator : NSObject
@end

@implementation MyEnumerator {
  NSEnumerator* _enumerator;
  Filter _filter;
}

- (instancetype)initWithArray:(NSArray*)array filter:(Filter)filter {
  _enumerator = [array objectEnumerator];
  _filter = filter;
  return self;
}

- (id)nextObject {
  id obj;
  while (obj = [_enumerator nextObject]) {
    if (_filter(obj)) {
      return obj;
    }
  }
  return NULL;
}

@end

@interface ArcEnumerator : NSObject
@end

@implementation ArcEnumerator

- (MyEnumerator*)makeMyEnumerator_zero:(NSArray*)x {
  return [[MyEnumerator alloc] initWithArray:x
                                      filter:^BOOL(NSObject* o) {
                                        NoArcCallee* dummy =
                                            [NoArcCallee giveMeObject];
                                        return true;
                                      }];
}

// The cost analyzer cannot reason the amortized complexity.
- (void)callMyEnumerator_linear_FP:(NSArray*)x {
  MyEnumerator* enumerator = [self makeMyEnumerator_zero:x];
  id s;
  while (s = [enumerator nextObject]) {
  }
}

- (void)callMyEnumerator_nextObject_linear:(NSArray*)x {
  MyEnumerator* enumerator = [self makeMyEnumerator_zero:x];
  id s = [enumerator nextObject];
}

@end
