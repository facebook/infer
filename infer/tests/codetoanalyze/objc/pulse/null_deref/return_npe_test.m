/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface ContainerClass : NSObject {
 @public
  int containedValue;
}

@end

@implementation ContainerClass

@end

@interface MyClass : NSObject

@end

@implementation MyClass

- (void)early_return_no_npe_good:(ContainerClass*)c {
  int i = 0;
  if (c == nil) {
    return;
  }

  if (i == 0) {
    // here c cannot be nil, because of the previous if
    i = c->containedValue;
  }
}

- (void)no_early_return_bad_latent_FN:(ContainerClass*)c {
  int i = 0;
  if (c == nil) {
  }

  if (i == 0) {
    i = c->containedValue;
  }
}

- (void)no_early_return_manifest_bad {
  [self no_early_return_bad_latent_FN:nil];
}

- (void)no_early_return_no_zero_latent_bad_FN:(ContainerClass*)c {
  int i = 1;
  if (c == nil) {
  }

  if (i == 1) {
    i = c->containedValue;
  }
}

- (void)no_early_return_no_zero_manifest_bad {
  [self no_early_return_no_zero_latent_bad_FN:nil];
}

@end
