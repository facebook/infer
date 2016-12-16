#import <UIKit/UIKit.h>

@interface Unavailable_api_in_supported_ios_sdk : NSObject

@end

@implementation Unavailable_api_in_supported_ios_sdk

- (void)test:(int)n and:(NSData*)data {
  NSDictionary* cacheData =
      [NSKeyedUnarchiver unarchiveTopLevelObjectWithData:data error:nil];
}
@end

static NSDictionary* OpenURLOptionsFromSourceApplication(
    NSString* sourceApplication) {
  NSDictionary* options =
      @{UIApplicationOpenURLOptionsSourceApplicationKey : sourceApplication};
  return options;
}
