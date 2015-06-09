#import <Security/Security.h>

SecCertificateRef __cf_alloc(SecCertificateRef);

SecCertificateRef __cf_non_null_alloc(SecCertificateRef);


SecCertificateRef SecCertificateCreateWithData ( CFAllocatorRef allocator,
                                                 CFDataRef data ) {
   SecCertificateRef c;
   return __cf_alloc(c);
}
