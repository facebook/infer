/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface Person : NSObject

@property(nullable, copy) Person* child;

@end

@interface User : NSObject

- (nullable User*)otherUser;

@property(strong, nonatomic) NSString* name;

@end

@implementation User

- (NSString*)otherUserNameNPEBad_FN {
  User* ou = [self otherUser];
  return ou->_name;
}

NSDictionary* npe_property_nullable_npe_bad_FN() {
  Person* person = [Person new];
  Person* child = person.child;
  return @{@"key" : child};
}

@end
