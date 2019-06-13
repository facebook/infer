/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSString.h>

typedef struct {
  const char* name;
} name_t;

static void CFunWithBlockOk(void (^cb)(void)) {
  name_t const* ns = NULL;
  name_t n = ns[0]; // Lfield accessor (needs to be specialized correctly)
  if (n.name != nil && cb != NULL)
    cb();
}

@interface A : NSObject
- (void)mOk;
@end

@implementation A
- (void)mOk {
  CFunWithBlockOk(^(void){
  });
}
@end
