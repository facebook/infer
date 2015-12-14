/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

BOOL ExampleSanitizer(NSURL * u, int f)
{
    if (f) __set_untaint_attribute(u);
    return f;
}

@interface ExampleViewController : NSObject
- (void)loadURL:(NSURL *)URL
  trackingCodes:(NSArray *)trackingCodes;
@end

@implementation ExampleViewController
- (void) dealloc
{
    [self dealloc];
}
- (void)loadURL:(NSURL *)URL
  trackingCodes: (NSArray *)trackingCodes
{
    // Require untainted URL
};
@end

@interface B : NSObject
- (void) another_url_pass: (NSURL *) u;
@end

@implementation B
- (void) dealloc
{
    [self dealloc];
}
- (void) another_url_pass: (NSURL *) u
{
    ExampleViewController *vc = [[ExampleViewController alloc] init];
    [vc loadURL:u trackingCodes:nil];
    [vc dealloc];
}
@end

@interface A : NSObject
- (void) pass_url_arond:(NSURL *) u;
@end

@implementation A
- (void) dealloc
{
    [self dealloc];
}
- (void) pass_url_arond: (NSURL *) u
{
    B* b = [[B alloc] init];
    [b another_url_pass:u];
    [b dealloc];
}
@end

@interface ExampleDelegate : NSObject
- (BOOL)application: (UIApplication *)application
            openURL: (NSURL *)URL
  sourceApplication: (NSString *)sourceApplication
         annotation: (id)annotation;
@end

@implementation ExampleDelegate
- (BOOL)application: (UIApplication *)application
            openURL: (NSURL *)URL
  sourceApplication: (NSString *)sourceApplication
         annotation: (id)annotation
{
    // Assume tainted URL;
    A* a = [[A alloc] init];
    if (!ExampleSanitizer(URL, 0 )) {
        [a pass_url_arond:URL]; // report taint
    }
    if (!ExampleSanitizer(URL, 1 )) {
        [a pass_url_arond:URL]; // No taint
    }
    [a dealloc];
    return YES;
}
@end
