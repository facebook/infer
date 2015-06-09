#import <Foundation/Foundation.h>

CFDictionaryRef __cf_alloc(CFDictionaryRef);
CFDictionaryRef __cf_non_null_alloc(CFDictionaryRef);

CFDictionaryRef CFPreferencesCopyMultiple(CFArrayRef keysToFetch,
                                          CFStringRef appName,
                                          CFStringRef user,
                                          CFStringRef host) {
  CFDictionaryRef c;
  return __cf_non_null_alloc(c);
}
