/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//
//  AppDelegate.m
//  HelloWorldApp
//

#import "AppDelegate.h"
#import "Hello.h"
#import <UIKit/UIKit.h>

@interface AppDelegate ()

@end

@implementation AppDelegate

- (void)memory_leak_bug {
  CGPathRef shadowPath = CGPathCreateWithRect(self.inputView.bounds, NULL);
}

- (void)resource_leak_bug {
  FILE* fp;
  fp = fopen("c:\\test.txt", "r");
}

- (void)parameter_not_null_checked_block_bug:(void (^)())callback {
  callback();
}

- (NSArray*)npe_in_array_literal_bug {
  NSString* str = nil;
  return @[ @"horse", str, @"dolphin" ];
}

- (NSArray*)premature_nil_termination_argument_bug {
  NSString* str = nil;
  return [NSArray arrayWithObjects:@"horse", str, @"dolphin", nil];
}

- (BOOL)application:(UIApplication*)application
    didFinishLaunchingWithOptions:(NSDictionary*)launchOptions {
  // Override point for customization after application launch.
  Hello* hello = [Hello new];
  [hello null_dereference_bug];
  [self memory_leak_bug];
  [self resource_leak_bug];
  [hello parameter_not_null_checked_bug:nil];
  [self parameter_not_null_checked_block_bug:nil];
  [hello ivar_not_nullable_bug:nil];
  [self npe_in_array_literal_bug];
  [self premature_nil_termination_argument_bug];
  return YES;
}

- (void)applicationWillResignActive:(UIApplication*)application {
  // Sent when the application is about to move from active to inactive state.
  // This can occur for certain types of temporary interruptions (such as an
  // incoming phone call or SMS message) or when the user quits the application
  // and it begins the transition to the background state.
  // Use this method to pause ongoing tasks, disable timers, and throttle down
  // OpenGL ES frame rates. Games should use this method to pause the game.
}

- (void)applicationDidEnterBackground:(UIApplication*)application {
  // Use this method to release shared resources, save user data, invalidate
  // timers, and store enough application state information to restore your
  // application to its current state in case it is terminated later.
  // If your application supports background execution, this method is called
  // instead of applicationWillTerminate: when the user quits.
}

- (void)applicationWillEnterForeground:(UIApplication*)application {
  // Called as part of the transition from the background to the inactive state;
  // here you can undo many of the changes made on entering the background.
}

- (void)applicationDidBecomeActive:(UIApplication*)application {
  // Restart any tasks that were paused (or not yet started) while the
  // application was inactive. If the application was previously in the
  // background, optionally refresh the user interface.
}

- (void)applicationWillTerminate:(UIApplication*)application {
  // Called when the application is about to terminate. Save data if
  // appropriate. See also applicationDidEnterBackground:.
}

@end
