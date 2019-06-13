/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface No_null_from_array : NSObject
@end

@implementation No_null_from_array {
  NSDictionary* _maps;
}

- (void)collectAvailableMaps {
  NSMutableDictionary* maps = [NSMutableDictionary dictionaryWithCapacity:100];
  NSString* filename = @"";
  NSArray* components = [filename componentsSeparatedByString:@"-"];
  NSString* locale = components[0];
  NSMutableDictionary* mapsForLocale = maps[locale];
  maps[locale] = @"";
}

@end
