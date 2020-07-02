/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface Test1 : NSObject

@property NSInteger x;

@end

@implementation Test1
@end

void iterate_upto_field_size_linear(Test1* test) {
  for (int ci = 0; ci < test.x; ci++) {
  }
}

@interface Person : NSObject
@property(nonatomic) NSString* name;
@property(nonatomic) NSUInteger bank_account;

- (id)init_with_name:(NSString*)name;
- (void)add_income:(NSUInteger)income;
+ (NSString*)species_name;

@end

@implementation Person

- (id)init_with_name_constant:(NSString*)name {
  self = [super init];
  if (self) {
    self.name = name;
    self.bank_account = (NSUInteger) @0;
  }
  return self;
}

- (void)add_income_constant:(NSUInteger)income {
  self.bank_account += income;
}

+ (NSString*)species_name_constant {
  return @"Human";
}

@end

NSString* create_common_person_constant() {
  Person* person = [[Person alloc] init_with_name_constant:@"John Smith"];
  [person add_income_constant:(NSUInteger) @100];
  return [Person species_name_constant];
}
