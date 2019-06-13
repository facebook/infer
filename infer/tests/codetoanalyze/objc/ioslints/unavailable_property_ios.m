/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
