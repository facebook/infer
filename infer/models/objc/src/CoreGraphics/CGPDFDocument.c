#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGPDFDocumentRef);

void CGPDFDocumentRelease ( CGPDFDocumentRef document ) {
    if (document) __objc_release_cf(document);
}

