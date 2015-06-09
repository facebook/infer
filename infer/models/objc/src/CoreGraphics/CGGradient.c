#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGGradientRef);

void CGGradientRelease ( CGGradientRef gradient ) {
    if (gradient) __objc_release_cf(gradient);
}

CGGradientRef __cf_non_null_alloc(CGGradientRef);

CGGradientRef CGGradientCreateWithColors ( CGColorSpaceRef space,
                                           CFArrayRef colors,
                                           const CGFloat locations[]) {
    CGGradientRef c;
    return __cf_non_null_alloc(c);
}

CGGradientRef CGGradientCreateWithColorComponents ( CGColorSpaceRef space,
                                                    const CGFloat components[],
                                                    const CGFloat locations[],
                                                    size_t count ) {
    CGGradientRef c;
    return __cf_non_null_alloc(c);
}
