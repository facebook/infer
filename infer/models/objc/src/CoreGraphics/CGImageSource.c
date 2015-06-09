#import <CoreGraphics/CoreGraphics.h>
#import <ImageIO/ImageIO.h>

CGImageSourceRef __cf_non_null_alloc(CGImageSourceRef);
CGImageSourceRef __cf_alloc(CGImageSourceRef);
void __objc_release_cf(CGImageSourceRef);

CGImageSourceRef CGImageSourceCreateWithData ( CFDataRef data, CFDictionaryRef options )
{
    CGImageSourceRef c;
    return __cf_non_null_alloc(c);
}

CGImageSourceRef CGImageSourceCreateWithDataProvider ( CGDataProviderRef provider, CFDictionaryRef options )
{
    CGImageSourceRef c;
    return __cf_non_null_alloc(c);
}

CGImageSourceRef CGImageSourceCreateWithURL ( CFURLRef url, CFDictionaryRef options )
{
    CGImageSourceRef c;
    return __cf_alloc(c);
}

CGImageSourceRef CGImageSourceCreateIncremental ( CFDictionaryRef options )
{
    CGImageSourceRef c;
    return __cf_non_null_alloc(c);
}

void CGImageSourceRelease ( CGImageSourceRef image )
{
    if (image) __objc_release_cf(image);
}


