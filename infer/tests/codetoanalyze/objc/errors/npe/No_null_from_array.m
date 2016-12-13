#import <Foundation/Foundation.h>

@interface No_null_from_array : NSObject
@end

@implementation No_null_from_array {
  NSDictionary* _maps;
}

- (void)collectAvailableMaps {
  NSMutableDictionary* maps = [NSMutableDictionary dictionaryWithCapacity:100];
  NSString* filename = @"";
  NSArray* components = [filename componentsSeparatedByString:@"-"];
  NSString* locale = components[0];
  NSMutableDictionary* mapsForLocale = maps[locale];
  maps[locale] = @"";
}

@end
