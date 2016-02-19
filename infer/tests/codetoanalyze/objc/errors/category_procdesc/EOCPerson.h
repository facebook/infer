/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

// Splitting EOCPerson into categories
#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

@interface EOCPerson : NSObject
@property(nonatomic, copy, readonly) NSString* firstName;
@property(nonatomic, copy, readonly) NSString* lastName;
@property(nonatomic, strong, readonly) NSArray* friends;

- (id)initWithFirstName:(NSString*)firstName andLastName:(NSString*)lastName;
@end

@interface EOCPerson (Friendship)
- (void)addFriend:(EOCPerson*)person;
- (void)removeFriend:(EOCPerson*)person;
- (BOOL)isFriendsWith:(EOCPerson*)person;
@end

@interface EOCPerson (Work)
- (void)performDaysWork;
- (void)takeVacationFromWork;
@end

@interface EOCPerson (Play)
- (void)goToTheCinema;
- (void)goToSportsGame;
@end
