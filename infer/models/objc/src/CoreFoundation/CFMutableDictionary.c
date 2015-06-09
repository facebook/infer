#import <ImageIO/ImageIO.h>
#import <CoreMedia/CoreMedia.h>

CFMutableDictionaryRef __cf_non_null_alloc(CFMutableDictionaryRef);

CFMutableDictionaryRef __cf_alloc(CFMutableDictionaryRef);

CFMutableDictionaryRef CFDictionaryCreateMutable ( CFAllocatorRef allocator,
                                                    CFIndex capacity,
                                                    const CFDictionaryKeyCallBacks *keyCallBacks,
                                                    const CFDictionaryValueCallBacks *valueCallBacks ) {

    CFMutableDictionaryRef c;
    return __cf_alloc(c);
}

CFMutableDictionaryRef CFDictionaryCreateMutableCopy ( CFAllocatorRef allocator,
                                                       CFIndex capacity,
                                                       CFDictionaryRef theDict ) {
    CFMutableDictionaryRef c;
    return __cf_non_null_alloc(c);
}
