/*
 * Copyright (c) 2015- Facebook.
 * All rights reserved.
 */

#import <Foundation/Foundation.h>
#import <CoreText/CTFont.h>


@interface TollBridgeExample : NSObject

@end

@implementation TollBridgeExample

- (void)bridgeTransfer {
    CFLocaleRef nameRef = CFLocaleCreate (NULL, NULL);
    NSLocale *a = (__bridge_transfer NSLocale *)nameRef;
}

- (void)bridge {
    CFLocaleRef nameRef = CFLocaleCreate (NULL, NULL);
    NSLocale *a = (__bridge NSLocale *)nameRef;
}


- (void)brideRetained {
    id observer = [NSLocale alloc];
    CFLocaleRef a = (__bridge_retained CFLocaleRef)observer;
}

- (void)_readHTTPHeader;
{
    CFDictionaryRef ref =  CFHTTPMessageCopyAllHeaderFields(NULL);
    CFBridgingRelease(ref);
}

CTFontRef cfautorelease_test() {
    return CFAutorelease(CTFontCreateWithName(CFSTR("Icon"), 17.0, NULL));
}

@end
