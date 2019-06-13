/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface Private : NSObject
- (void)write_ok:(int)data;
- (int)read_other_bad;
- (void)write_other_bad:(int)other_data;
@end
