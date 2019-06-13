/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface Person : NSObject

{
  NSString* personName;
  int personAge;
}

- (id)initWithName:(NSString*)name andAge:(int)age;

- (void)setAge:(int)age;

- (int)getAge;

@end

@implementation Person

- (id)initWithName:(NSString*)name andAge:(int)age {
  personName = name;
  personAge = age;
  return self;
}

- (void)setAge:(int)age {
  self->personAge = age;
}

- (int)getAge {
  return self->personAge;
}

@end

@interface Employee : Person

{
  NSString* employeeEducation;
}

- (id)initWithName:(NSString*)name
            andAge:(int)age
      andEducation:(NSString*)education;

- (void)setEmployeeEducation:(NSString*)employeeEducation;

@end

@implementation Employee

- (id)initWithName:(NSString*)name
            andAge:(int)age
      andEducation:(NSString*)education {
  if (self = [super initWithName:name andAge:age]) {
    employeeEducation = education;
  }
  int x = 1 / 0;
  return self;
}

- (void)setEmployeeEducation:(NSString*)employeeEducation {
  self->employeeEducation = employeeEducation;
}

@end

int testFields() {
  Employee* employee = [Employee new];
  [employee setEmployeeEducation:@"Master"];
  [employee setAge:29];
  [employee setEmployeeEducation:@"PhD"];
  return [employee getAge];
}

int subtyping_test() { return 1 / (testFields() - 29); }
