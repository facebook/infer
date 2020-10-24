/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

/* We model the file lookup as returning a constant-length collection
 * (T77961166). */
void contents_of_directory_at_url_constant(NSFileManager* file_manager,
                                           NSURL* url,
                                           NSArray* keys) {
  NSArray* array = [file_manager contentsOfDirectoryAtURL:url
                               includingPropertiesForKeys:keys
                                                  options:0
                                                    error:nil];
  for (int i = 0; i < array.count; i++) {
  }
}
