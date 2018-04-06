/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/NSObject.h>

@interface CaptureController : NSObject

@property(nonatomic, strong, readwrite) id delegate;

@end

@implementation CaptureController

@end

@interface CViewController : NSObject

@property(nonatomic, strong) CaptureController* captureController;

@end

@implementation CViewController

- (void)setCaptureInteractionController:(CaptureController*)captureController {
  if (_captureController != captureController) {
    _captureController.delegate = nil;
    _captureController = captureController;
    _captureController.delegate = self;
  }
}

@end
