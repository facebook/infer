/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

/* This is a simplified account of when the code of the function in reality
propagates the taint but Infer doesn't understand it, so we want to use the
propagator config instead.  */
NSURL* URLCreate(NSURL* nativeUrl, NSURL* fallbackUrl) {
  return [NSURL URLWithString:@""];
}

/* We don't have the code of this one, so the propagator config works in this
 * case. */
NSURL* URLCreate1(NSURL* nativeUrl, NSURL* fallbackUrl);

void logEvent(id event);

void propagate_taint_url_bad1(void) {
  NSURL* nativeUrl = [[NSURL alloc]
      initWithString:
          [NSString stringWithFormat:@""]]; // NSURL.initWithString: is a source
  NSURL* fallbackUrl =
      [[NSURL alloc] initWithString:[NSString stringWithFormat:@""]];
  NSURL* url = URLCreate(
      nativeUrl, fallbackUrl); // taint gets propagated thanks to the config

  logEvent(url);
}

void propagate_taint_url_bad2(void) {
  NSURL* nativeUrl = [[NSURL alloc]
      initWithString:
          [NSString stringWithFormat:@""]]; // NSURL.initWithString: is a source
  NSURL* fallbackUrl =
      [[NSURL alloc] initWithString:[NSString stringWithFormat:@""]];
  NSURL* url = URLCreate1(
      nativeUrl, fallbackUrl); // taint gets propagated thanks to the config

  logEvent(url);
}
