#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGPatternRef);

void CGPatternRelease ( CGPatternRef pattern ) {
    if (pattern) __objc_release_cf(pattern);
}

