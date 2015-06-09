#import <CoreGraphics/CoreGraphics.h>
#import <ImageIO/ImageIO.h>

CGImageDestinationRef __cf_non_null_alloc(CGImageDestinationRef);
CGImageDestinationRef __cf_alloc(CGImageDestinationRef);

CGImageDestinationRef CGImageDestinationCreateWithURL (CFURLRef url,
                                                       CFStringRef type,
                                                       size_t count,
                                                       CFDictionaryRef options) {
    CGImageDestinationRef c;
    return __cf_non_null_alloc(c);
}

CGImageDestinationRef CGImageDestinationCreateWithData ( CFMutableDataRef data,
                                                         CFStringRef type,
                                                         size_t count,
                                                         CFDictionaryRef options) {
    CGImageDestinationRef c;
    return __cf_non_null_alloc(c);
}
