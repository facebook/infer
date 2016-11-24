#include <Foundation/Foundation.h>
@interface Nsstring_length_no_npe : NSObject
@end

@implementation Nsstring_length_no_npe

- (NSDictionary*)logMessage:(NSString* __nullable)message {
  if (message.length > 0) {
    return @{ @"key" : message }; // No NPE because of model of NSString length.
  } else
    return nil;
}
@end
