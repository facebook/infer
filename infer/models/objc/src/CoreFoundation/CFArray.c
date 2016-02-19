/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <AddressBook/AddressBook.h>
#import <Foundation/Foundation.h>

CFArrayRef __cf_alloc(CFArrayRef);
CFArrayRef __cf_non_null_alloc(CFArrayRef);

CFArrayRef CFArrayCreate(CFAllocatorRef allocator,
                         const void** values,
                         CFIndex numValues,
                         const CFArrayCallBacks* callBacks) {
  CFArrayRef c;
  return __cf_alloc(c);
}

CFArrayRef CFNetworkCopyProxiesForURL(CFURLRef url,
                                      CFDictionaryRef proxySettings) {
  CFArrayRef c;
  return __cf_alloc(c);
}

CFArrayRef CFStringCreateArrayWithFindResults(
    CFAllocatorRef alloc,
    CFStringRef theString,
    CFStringRef stringToFind,
    CFRange rangeToSearch,
    CFStringCompareFlags compareOptions) {
  CFArrayRef c;
  return __cf_alloc(c);
}

CFArrayRef CFPreferencesCopyKeyList(CFStringRef applicationID,
                                    CFStringRef userName,
                                    CFStringRef hostName) {
  CFArrayRef c;
  return __cf_non_null_alloc(c);
}

CFArrayRef CNCopySupportedInterfaces(void) {
  CFArrayRef c;
  return __cf_non_null_alloc(c);
}

CFArrayRef ABAddressBookCopyArrayOfAllPeople(ABAddressBookRef addressBook) {
  CFArrayRef c;
  return __cf_non_null_alloc(c);
}
