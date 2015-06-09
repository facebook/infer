#import <Foundation/Foundation.h>
#import "SystemConfiguration/SCNetworkReachability.h"

SCNetworkReachabilityRef __cf_non_null_alloc(SCNetworkReachabilityRef);

SCNetworkReachabilityRef SCNetworkReachabilityCreateWithName (
                                                    CFAllocatorRef allocator,
                                                    const char *nodename ) {
    SCNetworkReachabilityRef c;
    return __cf_non_null_alloc(c);
}

SCNetworkReachabilityRef SCNetworkReachabilityCreateWithAddress ( CFAllocatorRef allocator,
                                                                  const struct sockaddr *address ) {
    SCNetworkReachabilityRef c;
    return __cf_non_null_alloc(c);
}
