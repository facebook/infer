/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

void __set_untaint_attribute(void*);

BOOL ExampleSanitizer(NSURL* u, int f) {
  if (f)
    __set_untaint_attribute(u);
  return f;
}

@interface ExampleViewController : NSObject
- (void)loadURL:(NSURL*)URL trackingCodes:(NSArray*)trackingCodes;
@end

@implementation ExampleViewController

- init {
  return self;
}

- (void)loadURL:(NSURL*)URL trackingCodes:(NSArray*)trackingCodes {
  // Require untainted URL
};
@end

@interface VCB : NSObject
- (void)another_url_pass:(NSURL*)u;
@end

@implementation VCB

- init {
  return self;
}

- (void)another_url_pass:(NSURL*)u {
  ExampleViewController* vc = [[ExampleViewController alloc] init];
  [vc loadURL:u trackingCodes:nil];
}
@end

@interface VCA : NSObject
- (void)pass_url_arond:(NSURL*)u;
@end

@implementation VCA

- init {
  return self;
}

- (void)pass_url_arond:(NSURL*)u {
  VCB* b = [[VCB alloc] init];
  [b another_url_pass:u];
}
@end

@interface ExampleDelegate : NSObject
- (BOOL)application:(UIApplication*)application
              openURL:(NSURL*)URL
    sourceApplication:(NSString*)sourceApplication
           annotation:(id)annotation;
@end

@implementation ExampleDelegate

- init {
  return self;
}

- (BOOL)application:(UIApplication*)application
              openURL:(NSURL*)URL
    sourceApplication:(NSString*)sourceApplication
           annotation:(id)annotation {
  // Assume tainted URL;
  VCA* a = [[VCA alloc] init];
  if (!ExampleSanitizer(URL, 0)) {
    [a pass_url_arond:URL]; // report taint
  }
  if (!ExampleSanitizer(URL, 1)) {
    [a pass_url_arond:URL]; // No taint
  }
  return YES;
}
@end
