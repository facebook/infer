#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGFontRef);

CGFontRef __cf_alloc(CGFontRef);

void CGFontRelease ( CGFontRef font ) {
    if (font) __objc_release_cf(font);
}

CGFontRef CGFontCreateWithDataProvider ( CGDataProviderRef provider ) {
    CGFontRef c;
    return __cf_alloc(c);
}
