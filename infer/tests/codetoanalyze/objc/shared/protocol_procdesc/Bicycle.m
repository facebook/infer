/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "Bicycle.h"
#import <Foundation/NSString.h>

@implementation Bicycle

- (void)signalStop {
  NSLog(@"Bending left arm downwards");
}
- (void)signalLeftTurn {
  NSLog(@"Extending left arm outwards");
}
- (void)signalRightTurn {
  NSLog(@"Bending left arm upwards");
}
- (void)startPedaling {
  NSLog(@"Here we go!");
}
- (void)removeFrontWheel {
  NSLog(
      @"Front wheel is off."
       "Should probably replace that before pedaling...");
}
- (void)lockToStructure:(id)theStructure {
  NSLog(@"Locked to structure. Don't forget the combination!");
}

@end
