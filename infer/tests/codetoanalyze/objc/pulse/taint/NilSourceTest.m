/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface Data

+ (NSArray<NSString*>*)data;

+ (NSArray<NSString*>*)nil_data;

@end

@implementation Data

+ (NSArray<NSString*>*)nil_data {
  return nil;
}

@end

void taint_data(NSString* s);

@interface NilSourceTest

@end

@implementation NilSourceTest

- (void)taintDataBad {
  NSArray<NSString*>* data = [Data data];
  NSString* item = [data count] > 0 ? data[0] : nil;
  taint_data(item);
}

- (void)taintNilDataGood {
  NSArray<NSString*>* data = [Data nil_data];
  NSString* item = [data count] > 0 ? data[0] : nil;
  taint_data(item);
}

@end
