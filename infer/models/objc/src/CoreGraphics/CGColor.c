#import <CoreGraphics/CoreGraphics.h>

CGColorRef __cf_non_null_alloc(CGColorRef);

void __objc_release_cf(CGColorRef);

void CGColorRelease (CGColorRef color) {
    if (color) __objc_release_cf(color);
}

CGColorRef CGColorCreate(CGColorSpaceRef space, const CGFloat components[]) {
    CGColorRef c;
    return __cf_non_null_alloc(c);
}

CGColorRef CGColorCreateCopyWithAlpha ( CGColorRef color, CGFloat alpha ) {
    CGColorRef c;
    return __cf_non_null_alloc(c);
}

//FB own code

CGColorRef FBColorCreateWithGray(CGFloat gray, CGFloat a) {
    CGColorRef c;
    return __cf_non_null_alloc(c);
}

CGColorRef FBColorCreateWithRGBA(uint8_t r, uint8_t g, uint8_t b, CGFloat a) {
    CGColorRef c;
    return __cf_non_null_alloc(c);
}

CGColorRef FBColorCreateWithRGB(uint8_t r, uint8_t g, uint8_t b) {
    CGColorRef c;
    return __cf_non_null_alloc(c);
}
