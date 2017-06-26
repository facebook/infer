/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <AVFoundation/AVFoundation.h>

@interface FNFPlayerLayer : CAEAGLLayer

- (instancetype)initWithConfigs:(FNFPlayerLayer*)configs;

@end

@implementation FNFPlayerLayer {
  BOOL my_field;
}

- (instancetype)initWithConfigs:(FNFPlayerLayer*)configs {
  my_field = configs.presentsWithTransaction;
  return self;
}

@end
