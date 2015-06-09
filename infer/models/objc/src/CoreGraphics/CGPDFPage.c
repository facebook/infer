#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGPDFPageRef);

void CGPDFPageRelease ( CGPDFPageRef page ) {
    if (page) __objc_release_cf(page);
}

