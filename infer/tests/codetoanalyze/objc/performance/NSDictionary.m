/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

// init dictionary

NSDictionary* nsdictionary_init_literal_constant() {
  NSDictionary* dict =
      @{@"helloString" : @"Hello, World!",
        @"magicNumber" : @42};
  return dict;
}

NSDictionary* nsdictionary_init_dictionary_constant() {
  return [NSDictionary dictionary];
}

void nsdictionary_init_with_dictionary_linear(NSDictionary* dict) {
  NSDictionary* copy_dict = [[NSDictionary alloc] initWithDictionary:dict];
  for (int i = 0; i < [copy_dict allValues].count; i++) {
  }
}

void nsdictionary_alloc_with_zone_init_with_dictionary_linear(
    NSDictionary* dict, NSZone* zone) {
  NSDictionary* copy_dict =
      [[NSDictionary allocWithZone:zone] initWithDictionary:dict];
  for (int i = 0; i < [copy_dict allValues].count; i++) {
  }
}

NSDictionary* nsdictionary_dictionary_with_objects_linear(int n_entries) {
  NSDictionary* asciiDict;
  NSString* keyArray[n_entries];
  NSNumber* valueArray[n_entries];
  NSInteger i;

  for (i = 0; i < n_entries; i++) {

    char charValue = 'a' + i;
    keyArray[i] = [NSString stringWithFormat:@"%c", charValue];
    valueArray[i] = [NSNumber numberWithChar:charValue];
  }

  asciiDict = [NSDictionary dictionaryWithObjects:(id*)valueArray
                                          forKeys:(id*)keyArray
                                            count:n_entries];

  for (int i = 0; i < [asciiDict.count integerValue]; i++) {
  }

  return asciiDict;
}

// accessing values and keys

void nsdictionary_all_keys_linear1(NSDictionary* dict) {
  for (int i = 0; i < [dict allKeys].count; i++) {
  }
}

void nsdictionary_all_keys_linear2(NSDictionary* dict) {
  NSArray* array = [dict allKeys];

  for (int i = 0; i < array.count; i++) {
  }
}

void nsdictionary_all_values_linear(NSDictionary* dict) {
  for (int i = 0; i < [dict allValues].count; i++) {
  }
}

id nsdictionary_find_key_constant(NSDictionary* dict, id item) {
  NSEnumerator* enumerator = [dict keyEnumerator];
  id key;
  while ((key = [enumerator nextObject]) && dict[key] == item) {
    return key;
  }
  return NULL;
}

// enumerate dictionary

void nsdictionary_fast_enumerate_linear(NSDictionary* dict) {
  for (id key in dict) {
  }
}

void nsdictionary_enumerate_constant() {
  NSDictionary* dict =
      @{@"helloString" : @"Hello, World!",
        @"magicNumber" : @42};

  for (NSString* key in dict) {
    id value = dict[key];
  }
}

void nsdictionary_enumerator_linear(NSDictionary* dict) {
  NSEnumerator* enumerator = [dict keyEnumerator];
  id key;
  while ((key = [enumerator nextObject])) {
  }
}

void nsdictionary_enumerate_call_constant() {
  NSDictionary* dict =
      @{@"helloString" : @"Hello, World!",
        @"magicNumber" : @42};

  nsdictionary_all_values_linear(dict);
}

void nsdictionary_dictionary_constant() {
  NSDictionary* dict = nsdictionary_init_dictionary_constant();
  nsdictionary_all_values_linear(dict);
}
