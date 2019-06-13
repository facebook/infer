/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSString.h>
#import <string>

std::string stdStringOK(NSString* s) {
  const char* s1 = [s UTF8String];
  if (s1) {
    std::string s2 = std::string([s UTF8String]);
    return s2;
  } else
    return "";
}

std::string stdStringBad(NSString* s) {
  std::string s2 = std::string([s UTF8String]);
  return s2;
}
