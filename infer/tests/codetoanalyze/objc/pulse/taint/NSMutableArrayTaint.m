/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface SensitiveData : NSObject

+ (NSArray*)getSensitiveData;

@end

void my_log(NSArray* array);

void add_object_propagator_bad() {
  NSArray* accounts = [SensitiveData getSensitiveData];
  NSMutableArray<NSString*>* targetAccounts = [NSMutableArray new];
  for (NSString* item in accounts) {
    NSString* newItem = item;
    [targetAccounts addObject:newItem];
  }
  my_log(targetAccounts);
}
