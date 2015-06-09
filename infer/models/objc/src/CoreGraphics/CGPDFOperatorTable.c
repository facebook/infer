#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGPDFOperatorTableRef);

void CGPDFOperatorTableRelease ( CGPDFOperatorTableRef table ) {
    if (table) __objc_release_cf(table);
}

