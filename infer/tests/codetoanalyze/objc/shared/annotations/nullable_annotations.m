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

- (nullable instancetype)initWithName:(nullable NSString*)name;

- (nullable NSString*)tellMeSomething;

- (nullable User*)otherUser;

@property(strong, nonatomic) NSString* name;

@end

@implementation User

- (nullable instancetype)initWithName:(nullable NSString*)name {
  return self;
}

- (nullable NSString*)tellMeSomething {
  return @"Hi";
}

- (NSString*)tellMeSomethingNotNullable {
  return @"Hi";
}

- (NSString*)tellMeSomething:(NSString*)s1
                         and:(NSString*)s2
                         and:(nullable NSString*)s3
                         and:(NSString*)s4 {
  return @"Hi";
}

- (NSString*)otherUserName {
  User* ou = [self otherUser];
  return ou->_name;
}

NSDictionary* npe_property_nullable() {
  Person* person = [Person new];
  Person* child = person.child;
  return @{@"key" : child};
}

@end
