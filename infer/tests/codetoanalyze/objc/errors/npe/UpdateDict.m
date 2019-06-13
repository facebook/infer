/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

void update_dict_with_null() {
  NSMutableDictionary* mDict =
      [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Matt",
                                                        @"firstName",
                                                        @"Galloway",
                                                        @"lastName",
                                                        @28,
                                                        @"age",
                                                        nil];
  mDict[@"firstName"] = nil;
}

void update_dict_with_key_null() {
  NSMutableDictionary* mDict =
      [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Matt",
                                                        @"firstName",
                                                        @"Galloway",
                                                        @"lastName",
                                                        @28,
                                                        @"age",
                                                        nil];
  id key = nil;
  mDict[key] = @"Dulma";
}

void update_dict_without_null() {
  NSMutableDictionary* mDict =
      [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Matt",
                                                        @"firstName",
                                                        @"Galloway",
                                                        @"lastName",
                                                        @28,
                                                        @"age",
                                                        nil];
  mDict[@"firstName"] = @"Dulma";
}

void update_array_with_null() {
  NSMutableArray* array =
      [NSMutableArray arrayWithObjects:@"Dulma", @"Rodriguez", nil];
  NSUInteger idx = 0;
  id newObject = nil;
  array[idx] = newObject;
}

void update_array_without_null() {
  NSMutableArray* array =
      [NSMutableArray arrayWithObjects:@"Dulma", @"Rodriguez", nil];
  NSUInteger idx = 0;
  id newObject = @"Dino";
  array[idx] = newObject;
}

void add_nil_to_array() {
  NSMutableArray* array =
      [NSMutableArray arrayWithObjects:@"Dulma", @"Rodriguez", nil];
  id str = nil;
  [array addObject:str];
}

void insert_nil_in_array() {
  NSMutableArray* array =
      [NSMutableArray arrayWithObjects:@"Dulma", @"Rodriguez", nil];
  id str = nil;
  [array insertObject:str atIndex:0];
}

void add_nil_in_dict() {
  NSMutableDictionary* mDict =
      [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Matt",
                                                        @"firstName",
                                                        @"Galloway",
                                                        @"lastName",
                                                        @28,
                                                        @"age",
                                                        nil];
  id str = nil;
  [mDict setObject:str forKey:@"firstName"];
}

void no_npe_for_undef_values(NSDictionary* response) {
  NSMutableDictionary* fileInfo = [response mutableCopy];
  fileInfo[@"type"] = @"BLA";
  NSMutableDictionary* d = [NSMutableDictionary dictionary];
  d[@"fds"] = fileInfo;
}

void nullable_NSDictionary_objectForKey(NSDictionary* dict, NSObject* key) {
  if (key != nil) {
    // check objectForKey may return nil for non-nil key
    if ([dict objectForKey:key] == nil) {
      char _ = *(char*)nil;
    } // report
  }
}

void strict_NSDictionary_objectForKey(NSDictionary* dict) {
  // check objectForKey returns nil for nil key
  if ([dict objectForKey:nil] != nil) {
    char _ = *(char*)nil;
  } // no report
}

void nullable_NSDictionary_objectForKeyedSubscript(NSDictionary* dict,
                                                   NSObject* key) {
  if (key != nil) {
    // check objectForKey may return nil for non-nil key
    if (dict[key] == nil) {
      char _ = *(char*)nil;
    } // report
  }
}

void strict_NSDictionary_objectForKeyedSubscript(NSDictionary* dict) {
  // check objectForKey returns nil for nil key
  if (dict[nil] != nil) {
    char _ = *(char*)nil;
  } // no report
}

void nullable_NSMapTable_objectForKey(NSMapTable* map, NSObject* key) {
  if (key != nil) {
    // check objectForKey may return nil for non-nil key
    if ([map objectForKey:key] == nil) {
      char _ = *(char*)nil;
    } // report
  }
}

void strict_NSMapTable_objectForKey(NSMapTable* map) {
  // check objectForKey returns nil for nil key
  if ([map objectForKey:nil] != nil) {
    char _ = *(char*)nil;
  } // no report
}
