/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>
@interface MyClass : NSObject
@end
@implementation MyClass
- (void)sink {
}
- (void)not_si_nk {
}
- (void)sanitizer {
  [self sink];
}
- (void)not_sani_tizer {
  [self sink];
}
- (void)source1_Ok {
  [self not_si_nk];
}
- (void)source2_Bad {
  [self sink];
}
- (void)source3_Ok {
  [self sanitizer];
}
- (void)source4_Bad {
  [self not_sani_tizer];
}
@end
