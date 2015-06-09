#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGColorSpaceRef);

void CGColorSpaceRelease ( CGColorSpaceRef space ) {
    if (space) __objc_release_cf(space);
}

CGColorSpaceRef __cf_alloc(CGColorSpaceRef);

CGColorSpaceRef CGColorSpaceCreateDeviceRGB ( void ) {
    CGColorSpaceRef c;
    return __cf_alloc(c);
}

CGColorSpaceRef CGColorSpaceCreateDeviceGray ( void ) {
    CGColorSpaceRef c;
    return __cf_alloc(c);
}
