/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@class ComputerUser;

@interface UserData : NSObject

@end

@implementation UserData {
 @public
  ComputerUser* _user;
}
@end

@interface ComputerUser : NSObject

@end

@implementation ComputerUser {
 @public
  UserData* _data;
}
@end

@interface Project
@end

@implementation Project {
 @public
  UserData* _data;
 @public
  ComputerUser* _user;
}
@end

int test_bad(Project* project) {
  UserData* d = project->_data;
  d->_user = project->_user;
  d->_user->_data = d;
}
