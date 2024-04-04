/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface MyData : NSObject

- (NSString*)data;

@end

@implementation MyData

- (NSString*)data {
  return @"";
}

@end

@interface Logger : NSObject

@end

@interface Engine : NSObject

+ (void)process:(NSString*)data logger:(Logger*)logger;

+ (void)log:(NSString*)data;

@end

void process_data_no_log_ok() {
  MyData* d = [MyData new];
  [Engine process:[d data] logger:nil];
}

void process_data_log_bad() {
  MyData* d = [MyData new];
  [Engine log:[d data]];
}
