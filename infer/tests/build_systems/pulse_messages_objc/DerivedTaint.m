/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface University : NSObject

@property(nonatomic, strong) NSString* name;

- (instancetype)initWithName:(NSString*)name;
@end

@implementation University

- (instancetype)initWithName:(NSString*)name {
  _name = name;
}

@end

@interface Student : NSObject

@property(nonatomic, strong) University* university;

- (instancetype)initWithUniversity:(University*)university;

@end

@implementation Student

- (instancetype)initWithUniversity:(University*)university {
  _university = university;
}

@end

void my_log(NSObject* obj) {}

void my_source(NSObject* name) {
  int* p = NULL;
  *p = 42;
}

void test_derived_taint_bad1() {
  University* u = [[University alloc] initWithName:@"LMU"];
  Student* p = [[Student alloc] initWithUniversity:u];
  my_source(p.university);
  my_log(p);
}

void test_derived_taint_bad2() {
  University* u = [[University alloc] initWithName:@"LMU"];
  Student* p = [[Student alloc] initWithUniversity:u];
  my_source(p.university.name);
  my_log(p);
}

void test_derived_taint_bad3() {
  University* u = [[University alloc] initWithName:@"LMU"];
  Student* p = [[Student alloc] initWithUniversity:u];
  my_source(p.university.name);
  my_log(p.university);
}
