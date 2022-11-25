/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

void logEvent(id event);

static void stringPropagatesTaintBad() {
  NSURL* url = [[NSURL alloc] initWithString:@""];
  NSString* urlInfo =
      [NSString stringWithFormat:@"%@://%@", url.scheme, url.host];
  logEvent(urlInfo);
}
