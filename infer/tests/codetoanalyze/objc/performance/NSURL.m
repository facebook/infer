/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

void get_resource_value_constant(NSURL* url) {
  BOOL b = [url getResourceValue:nil forKey:@"key" error:nil];
  for (int i = 0; i < b; i++) {
  }
}

void path_linear(NSURL* url) {
  NSString* path = (NSString*)url.path;
  for (int i = 0; i < path.length; i++) {
  }
}
