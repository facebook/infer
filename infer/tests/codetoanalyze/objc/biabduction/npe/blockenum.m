/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface BlockEnumB : NSObject
+ foo:(id)obj;
@end

@implementation BlockEnumB

+ (BlockEnumB*)foo:(id)obj {
  return obj;
}

@end

@interface BlockEnumA : NSObject
- (NSMutableArray*)allResultsList:(NSArray*)allResults;
@end

@implementation BlockEnumA

// From a diff
- (NSMutableArray*)allResultsList:(NSArray*)allResults {
  NSMutableArray* resultsList = [[NSMutableArray alloc] init];
  [allResults enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL* stop) {
    id* result = [BlockEnumB foo:obj];
    if (result != nil) {
      [resultsList addObject:result];
    }
  }];
  return resultsList;
}

// How that code is translated in INFER
- (void)foo1:(NSArray*)a {
  NSArray* objects = a;
  NSMutableArray* resultsList = [[NSMutableArray alloc] init];
  void (^enumerateObjectsUsingBlock)(id, NSUInteger, BOOL*) =
      ^(id obj, NSUInteger idx, BOOL* stop) {
        id* result = [BlockEnumB foo:obj];
        if (result != nil) {
          [resultsList addObject:result];
        }
      };
  BOOL* stop = malloc(sizeof(BOOL));
  *stop = NO;
  for (NSUInteger idx = 0; idx < objects.count; idx++) {
    id object = objects[idx];
    enumerateObjectsUsingBlock(object, idx, stop);
    if (*stop == YES)
      break;
  }
  free(stop);
}

@end
