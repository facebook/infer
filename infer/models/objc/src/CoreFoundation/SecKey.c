#import <Security/SecKey.h>
#import <Security/SecTrust.h>

SecKeyRef __cf_alloc(SecKeyRef);

SecPolicyRef __cf_non_null_alloc(SecPolicyRef);

SecKeyRef SecTrustCopyPublicKey ( SecTrustRef trust ){
    SecKeyRef c;
    return __cf_alloc(c);
}


SecPolicyRef SecPolicyCreateSSL ( Boolean server, CFStringRef hostname ) {
   SecPolicyRef c;
   return __cf_non_null_alloc(c);
}

SecPolicyRef SecPolicyCreateBasicX509 ( void ) {
   SecPolicyRef c;
   return __cf_non_null_alloc(c);
}
