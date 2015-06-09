/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import "StreetVehicle.h"

@interface Bicycle : NSObject<StreetVehicle>

- (void)startPedaling;
- (void)removeFrontWheel;
- (void)lockToStructure:(id)theStructure;

@end
