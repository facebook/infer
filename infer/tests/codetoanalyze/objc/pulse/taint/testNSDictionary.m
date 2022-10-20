/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

NSDictionary* tainted_dictionary(void);
void* create_tainted(void);
id create_tainted_id(void);
void* create_untainted(void);
id create_untainted_id(void);
void testNSDictionary_sink(void* ptr) {
  if (ptr) {
    *ptr;
  }
}

void testNSDictionary_DictionaryWithObjectsForKeysBad(void) {
  NSArray* arg = (__bridge NSArray* _Nonnull)(create_tainted());
  NSDictionary* propagated = [NSDictionary dictionaryWithObjects:arg
                                                         forKeys:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_DictionaryWithObjectsForKeysCountBad(void) {
  id count = create_tainted_id();
  id const* arg = &count;
  NSDictionary* propagated = [NSDictionary dictionaryWithObjects:arg
                                                         forKeys:arg
                                                           count:count];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_InitWithObjectsForKeysBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  NSArray* arg = (__bridge NSArray* _Nonnull)(create_tainted());
  NSDictionary* propagated = [dictionary initWithObjects:arg forKeys:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_InitWithObjectsForKeysGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSArray* arg = [NSArray new];
  [dictionary initWithObjects:arg forKeys:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_InitWithObjectsForKeysCountBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  id count = create_tainted_id();
  id const* arg = &count;
  NSDictionary* propagated = [dictionary initWithObjects:arg
                                                 forKeys:arg
                                                   count:count];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_InitWithObjectsForKeysCountGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  id count = create_untainted_id();
  [dictionary initWithObjects:&count forKeys:&count count:count];
  testNSDictionary_sink(&count);
}

void testNSDictionary_DictionaryWithObjectsAndKeysBad(void) {
  NSArray* arg = (__bridge NSArray* _Nonnull)(create_tainted());
  NSDictionary* propagated = [NSDictionary dictionaryWithObjectsAndKeys:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_InitWithObjectsAndKeysBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  NSArray* arg = (__bridge NSArray* _Nonnull)(create_tainted());
  NSDictionary* propagated = [dictionary initWithObjectsAndKeys:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_InitWithObjectsAndKeysGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSArray* arg = [NSArray new];
  [dictionary initWithObjectsAndKeys:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_DictionaryWithObjectForKeyBad(void) {
  id arg = create_tainted_id();
  NSDictionary* propagated = [NSDictionary dictionaryWithObject:arg forKey:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_DictionaryWithDictionaryBad(void) {
  NSDictionary* arg = (__bridge NSDictionary* _Nonnull)(create_tainted());
  NSDictionary* propagated = [NSDictionary dictionaryWithDictionary:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_InitWithDictionaryBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  NSDictionary* arg = (__bridge NSDictionary* _Nonnull)(create_tainted());
  NSDictionary* propagated = [dictionary initWithDictionary:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_InitWithDictionaryGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSDictionary* arg = [NSDictionary new];
  [dictionary initWithDictionary:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_InitWithDictionaryCopyItemsBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  NSDictionary* arg = (__bridge NSDictionary* _Nonnull)(create_tainted());
  NSDictionary* propagated = [dictionary initWithDictionary:arg
                                                  copyItems:false];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_InitWithDictionaryCopyItemsGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSDictionary* arg = [NSDictionary new];
  [dictionary initWithDictionary:arg copyItems:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_DictionaryWithContentsOfURLErrorBad(void) {
  NSURL* arg = (__bridge NSURL* _Nonnull)(create_tainted());
  NSError* error = [NSError new];
  NSDictionary* propagated = [NSDictionary dictionaryWithContentsOfURL:arg
                                                                 error:&error];
  testNSDictionary_sink((__bridge void*)(error));
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_InitWithCoderBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  NSCoder* arg = (__bridge NSCoder* _Nonnull)(create_tainted());
  NSDictionary* propagated = [dictionary initWithCoder:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_InitWithCoderGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSCoder* arg = [NSCoder new];
  [dictionary initWithCoder:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_SharedKeySetForKeysBad(void) {
  NSCoder* arg = (__bridge NSCoder* _Nonnull)(create_tainted());
  id propagated = [NSDictionary sharedKeySetForKeys:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_CountBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSUInteger propagated = [dictionary count];
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_IsEqualToDictionaryBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  NSDictionary* arg = tainted_dictionary();
  bool propagated = [dictionary isEqualToDictionary:arg];
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_IsEqualToDictionaryGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSDictionary* arg = [NSDictionary new];
  [dictionary isEqualToDictionary:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_AllKeysBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSArray* propagated = [dictionary allKeys];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_AllKeysForObjectBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  id arg = create_tainted_id();
  NSDictionary* propagated = [dictionary allKeysForObject:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_AllKeysForObjectGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  id arg = create_untainted_id();
  [dictionary allKeysForObject:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_AllValuesBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSArray* propagated = [dictionary allValues];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_ValueForKeyBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  NSString* arg = (__bridge NSString* _Nonnull)(create_tainted());
  NSDictionary* propagated = [dictionary valueForKey:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_ValueForKeyGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSString* arg = [NSString new];
  [dictionary valueForKey:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_GetObjectsAndKeysCountBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  id arg = create_tainted_id();
  id untainted = create_untainted_id();
  __unsafe_unretained id arg2 = untainted;
  [dictionary getObjects:&arg2 andKeys:&arg2 count:arg];
  testNSDictionary_sink(&arg2);
}

void testNSDictionary_GetObjectsAndKeysCountGood(void) {
  NSDictionary* dictionary = [NSDictionary new];
  id arg = create_untainted_id();
  id tainted = create_tainted_id();
  __unsafe_unretained id arg2 = tainted;
  [dictionary getObjects:&arg2 andKeys:&arg2 count:arg];
  testNSDictionary_sink((__bridge void*)(arg));
  testNSDictionary_sink((__bridge void*)(dictionary));
}

void testNSDictionary_ObjectsForKeysNotFoundMarkerBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  NSArray* arg = (__bridge NSArray* _Nonnull)(create_tainted());
  NSDictionary* propagated = [dictionary objectsForKeys:arg
                                         notFoundMarker:create_tainted_id()];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_ObjectsForKeysNotFoundMarkerGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSArray* arg = [NSArray new];
  [dictionary objectsForKeys:arg notFoundMarker:0];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_ObjectForKeyBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  id arg = create_tainted_id();
  NSDictionary* propagated = [dictionary objectForKey:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_ObjectForKeyGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  id arg = create_untainted_id();
  [dictionary objectForKey:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_ObjectForKeyedSubscriptBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  id arg = create_tainted_id();
  NSDictionary* propagated = [dictionary objectForKeyedSubscript:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_ObjectForKeyedSubscriptGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  id arg = create_untainted_id();
  [dictionary objectForKeyedSubscript:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_KeyEnumeratorBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSEnumerator* propagated = [dictionary keyEnumerator];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_ObjectEnumeratorBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSEnumerator* propagated = [dictionary objectEnumerator];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_EnumerateKeysAndObjectsUsingBlockBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  void* arg = create_tainted();
  [dictionary enumerateKeysAndObjectsUsingBlock:(__bridge void (^_Nonnull)(
                                                    id _Nonnull __strong,
                                                    id _Nonnull __strong,
                                                    BOOL* _Nonnull))(arg)];
  testNSDictionary_sink((__bridge void*)(dictionary));
}

void testNSDictionary_EnumerateKeysAndObjectsUsingBlockGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  void* arg = create_untainted();
  [dictionary enumerateKeysAndObjectsUsingBlock:(__bridge void (^_Nonnull)(
                                                    id _Nonnull __strong,
                                                    id _Nonnull __strong,
                                                    BOOL* _Nonnull))(arg)];
  testNSDictionary_sink(arg);
}

void testNSDictionary_EnumerateKeysAndObjectsWithOptionsUsingBlockBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  void* arg = create_tainted();
  [dictionary enumerateKeysAndObjectsWithOptions:arg
                                      usingBlock:(__bridge void (^_Nonnull)(
                                                     id _Nonnull __strong,
                                                     id _Nonnull __strong,
                                                     BOOL* _Nonnull))(arg)];
  testNSDictionary_sink((__bridge void*)(dictionary));
}

void testNSDictionary_EnumerateKeysAndObjectsWithOptionsUsingBlockGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  void* arg = create_untainted();
  [dictionary enumerateKeysAndObjectsWithOptions:arg
                                      usingBlock:(__bridge void (^_Nonnull)(
                                                     id _Nonnull __strong,
                                                     id _Nonnull __strong,
                                                     BOOL* _Nonnull))(arg)];
  testNSDictionary_sink(arg);
}

void testNSDictionary_CountByEnumeratingWithStateObjectsCountBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  void* arg = create_tainted();
  id untainted = create_untainted_id();
  __unsafe_unretained id arg2 = untainted;
  NSUInteger propagated = [dictionary countByEnumeratingWithState:arg
                                                          objects:&arg2
                                                            count:arg];
  testNSDictionary_sink(&propagated);
  testNSDictionary_sink(&arg2);
}

void testNSDictionary_CountByEnumeratingWithStateObjectsCountGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  void* arg = create_untainted();
  id tainted = create_tainted_id();
  __unsafe_unretained id arg2 = tainted;
  [dictionary countByEnumeratingWithState:arg objects:&arg2 count:arg];
  testNSDictionary_sink(arg);
}

void testNSDictionary_KeysSortedByValueUsingSelectorBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  void* arg = create_tainted();
  NSDictionary* propagated = [dictionary keysSortedByValueUsingSelector:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_KeysSortedByValueUsingSelectorGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  void* arg = create_untainted();
  [dictionary keysSortedByValueUsingSelector:arg];
  testNSDictionary_sink(arg);
}

void testNSDictionary_KeysSortedByValueUsingComparatorBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  void* arg = create_tainted();
  NSDictionary* propagated = [dictionary
      keysSortedByValueUsingComparator:(__bridge NSComparator _Nonnull)(arg)];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_KeysSortedByValueUsingComparatorGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  void* arg = create_untainted();
  [dictionary
      keysSortedByValueUsingComparator:(__bridge NSComparator _Nonnull)(arg)];
  testNSDictionary_sink(arg);
}

void testNSDictionary_KeysSortedByValueWithOptionsUsingComparatorBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  void* arg = create_tainted();
  NSDictionary* propagated = [dictionary
      keysSortedByValueWithOptions:arg
                   usingComparator:(__bridge NSComparator _Nonnull)(arg)];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_KeysSortedByValueWithOptionsUsingComparatorGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  void* arg = create_untainted();
  [dictionary
      keysSortedByValueWithOptions:arg
                   usingComparator:(__bridge NSComparator _Nonnull)(arg)];
  testNSDictionary_sink(arg);
}

void testNSDictionary_KeysOfEntriesPassingTestBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  void* arg = create_tainted();
  NSDictionary* propagated = [dictionary
      keysOfEntriesPassingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                         id _Nonnull __strong,
                                                         BOOL* _Nonnull))(arg)];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_KeysOfEntriesPassingTestGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  void* arg = create_untainted();
  [dictionary
      keysOfEntriesPassingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                         id _Nonnull __strong,
                                                         BOOL* _Nonnull))(arg)];
  testNSDictionary_sink(arg);
}

void testNSDictionary_KeysOfEntriesWithOptionsPassingTestBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  void* arg = create_tainted();
  NSDictionary* propagated = [dictionary
      keysOfEntriesWithOptions:arg
                   passingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                         id _Nonnull __strong,
                                                         BOOL* _Nonnull))(arg)];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_KeysOfEntriesWithOptionsPassingTestGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  void* arg = create_untainted();
  [dictionary
      keysOfEntriesWithOptions:arg
                   passingTest:(__bridge BOOL(^_Nonnull)(id _Nonnull __strong,
                                                         id _Nonnull __strong,
                                                         BOOL* _Nonnull))(arg)];
  testNSDictionary_sink(arg);
}

void testNSDictionary_WriteToURLErrorBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  void* arg = create_untainted();
  NSError* error = [NSError new];
  BOOL propagated =
      [dictionary writeToURL:(__bridge NSURL* _Nonnull)(arg) error:&error];
  testNSDictionary_sink(arg);
  testNSDictionary_sink((__bridge void*)(error));
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_WriteToURLErrorGood(void) {
  NSDictionary* dictionary = [NSDictionary new];
  void* arg = create_tainted();
  NSError* error = [NSError new];
  [dictionary writeToURL:(__bridge NSURL* _Nonnull)(arg) error:&error];
  testNSDictionary_sink((__bridge void*)(dictionary));
}

void testNSDictionary_FileSizeBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  unsigned long long propagated = [dictionary fileSize];
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_FileTypeBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSString* propagated = [dictionary fileType];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_FileCreationDateBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSDate* propagated = [dictionary fileCreationDate];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_FileModificationDateBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSDate* propagated = [dictionary fileModificationDate];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_FilePosixPermissionsBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSUInteger propagated = [dictionary filePosixPermissions];
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_FileOwnerAccountIDBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSNumber* propagated = [dictionary fileOwnerAccountID];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_FileOwnerAccountNameBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSString* propagated = [dictionary fileOwnerAccountName];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_FileGroupOwnerAccountIDBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSNumber* propagated = [dictionary fileGroupOwnerAccountID];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_FileGroupOwnerAccountNameBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSString* propagated = [dictionary fileGroupOwnerAccountName];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_FileExtensionHiddenBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  BOOL propagated = [dictionary fileExtensionHidden];
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_FileIsImmutableBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  BOOL propagated = [dictionary fileIsImmutable];
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_FileIsAppendOnlyBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  BOOL propagated = [dictionary fileIsAppendOnly];
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_FileSystemFileNumberBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSUInteger propagated = [dictionary fileSystemFileNumber];
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_FileSystemNumberBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSInteger propagated = [dictionary fileSystemNumber];
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_FileHFSTypeCodeBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  OSType propagated = [dictionary fileHFSTypeCode];
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_FileHFSCreatorCodeBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  OSType propagated = [dictionary fileHFSCreatorCode];
  testNSDictionary_sink(&propagated);
}

void testNSDictionary_DescriptionBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSString* propagated = [dictionary description];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_DescriptionInStringsFileFormatBad(void) {
  NSDictionary* dictionary = tainted_dictionary();
  NSString* propagated = [dictionary descriptionInStringsFileFormat];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_DescriptionWithLocaleBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  id arg = create_tainted_id();
  NSDictionary* propagated = [dictionary descriptionWithLocale:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_DescriptionWithLocaleGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  id arg = create_untainted_id();
  [dictionary descriptionWithLocale:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}

void testNSDictionary_DescriptionWithLocaleIndentBad(void) {
  NSDictionary* dictionary = [NSDictionary new];
  id arg = create_tainted_id();
  NSDictionary* propagated = [dictionary descriptionWithLocale:arg indent:arg];
  testNSDictionary_sink((__bridge void*)(propagated));
}

void testNSDictionary_DescriptionWithLocaleIndentGood(void) {
  NSDictionary* dictionary = tainted_dictionary();
  id arg = create_untainted_id();
  [dictionary descriptionWithLocale:arg indent:arg];
  testNSDictionary_sink((__bridge void*)(arg));
}
