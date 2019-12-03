/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>
#import <string>

@interface A : NSObject {
}
- (char _Nullable*)toString;
@end

std::string stdStringOK(A* a) {
  const char* s1 = [a toString];
  if (s1) {
    std::string s2 = std::string(s1);
    return s2;
  } else
    return "";
}

std::string stdStringBad(A* a) {
  std::string s2 = std::string([a toString]);
  return s2;
}
