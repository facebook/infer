/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@protocol StreetVehicle <NSObject>


- (void)signalStop;
- (void)signalLeftTurn;
- (void)signalRightTurn;


@end
