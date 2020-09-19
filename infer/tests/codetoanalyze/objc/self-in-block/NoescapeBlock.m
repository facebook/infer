/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface B : NSObject
@end

@interface ArrayUtils : NSObject

+ (void)enumerate:(void (^)(id obj, NSUInteger idx, BOOL* stop))block;
@end

@interface A : NSObject
- (NSMutableArray*)allResultsList:(NSArray<B*>*)allResults;

- (B*)process:(B*)obj;
@end

@implementation A {
  int x;
}

- (B*)process:(B*)obj {
  return obj;
}

- (NSMutableArray<B*>*)weak_in_noescape_block_bad:(NSArray<B*>*)allResults {
  NSMutableArray<B*>* resultsList = [[NSMutableArray<B*> alloc] init];
  __weak __typeof(self) weakSelf = self;
  [allResults enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL* stop) {
    B* result = [weakSelf process:obj]; // bug
    B* result1 = [weakSelf process:obj]; // no bug because of dedup
    if (result != nil) {
      [resultsList addObject:result];
    }
  }];
  return resultsList;
}

- (NSMutableArray<B*>*)weak_in_noescape_block_good:(NSArray<B*>*)allResults {
  NSMutableArray<B*>* resultsList = [[NSMutableArray<B*> alloc] init];
  [allResults enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL* stop) {
    B* result = [self process:obj]; // no bug
    if (result != nil) {
      [resultsList addObject:result];
    }
  }];
  return resultsList;
}

- (NSMutableArray<B*>*)weak_in_noescape_block1_good:(NSArray<B*>*)allResults {
  NSMutableArray<B*>* resultsList = [[NSMutableArray<B*> alloc] init];
  __weak __typeof(self) weakSelf = self;
  [ArrayUtils enumerate:^(id obj, NSUInteger idx, BOOL* stop) {
    B* result = [weakSelf process:obj]; // no bug
    if (result != nil) {
      [resultsList addObject:result];
    }
  }];
  return resultsList;
}

- (NSMutableArray<B*>*)weak_in_block_inside_noescape_block_good:
    (NSArray<B*>*)allResults {
  NSMutableArray<B*>* resultsList = [[NSMutableArray alloc] init];
  __weak __typeof(self) weakSelf = self;
  [allResults enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL* stop) {
    B* result = [self process:obj];
    if (result != nil) {
      [resultsList addObject:result];
    }
  }];
  [ArrayUtils enumerate:^(id obj, NSUInteger idx, BOOL* stop) {
    __strong __typeof(weakSelf) strongSelf = weakSelf; // no bug here
    if (strongSelf) {
      int x = strongSelf->x;
    }
  }];
  return resultsList;
}

@end
