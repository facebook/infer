/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <CoreText/CTFont.h>
#import <Foundation/Foundation.h>

@interface TollBridgeExample : NSObject

@end

@implementation TollBridgeExample

- (void)bridgeTransfer {
  CFLocaleRef nameRef = CFLocaleCreate(NULL, NULL);
  NSLocale* a = (__bridge_transfer NSLocale*)nameRef;
}

- (void)bridge {
  CFLocaleRef nameRef = CFLocaleCreate(NULL, NULL);
  NSLocale* a = (__bridge NSLocale*)nameRef;
}

- (void)brideRetained {
  id observer = [NSLocale alloc];
  CFLocaleRef a = (__bridge_retained CFLocaleRef)observer;
}

- (void)_readHTTPHeader;
{
  CFDictionaryRef ref = CFHTTPMessageCopyAllHeaderFields(NULL);
  CFBridgingRelease(ref);
}

CTFontRef cfautorelease_test() {
  return CFAutorelease(CTFontCreateWithName(CFSTR("Icon"), 17.0, NULL));
}

void bridgeDictionaryNoLeak() {
  NSDictionary* bufferAttributes = @{(NSString*)@"key" : @{}};
  CFDictionaryRef dict = (__bridge CFDictionaryRef)bufferAttributes;
}

@end
