#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGPDFContentStreamRef);

void CGPDFContentStreamRelease ( CGPDFContentStreamRef cs ) {
    if (cs) __objc_release_cf(cs);
}

