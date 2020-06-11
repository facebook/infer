This check warns you when you have a custom setter for a weak property. When
compiled with Automatic Reference Counting (ARC, `-fobj-arc`) ARC may set the
property to `nil` without invoking the setter, for example:

```objectivec
#import <Foundation/Foundation.h>

@interface Employee : NSObject {
  NSString* _name;
  __weak Employee* _manager;
}
-(id)initWithName:(NSString*)name;
@property(atomic, weak) Employee* manager;
-(void)report;
@end

@implementation Employee

-(id)initWithName:(NSString*)name {
  _name = name;
  return self;
}

-(NSString*)description {
  return _name;
}

-(void)report {
  NSLog(@"I work for %@", _manager);
}

-(Employee*)manager {
  return _manager;
}

// DON'T do this; ARC will not call this when setting _manager to nil.
-(void)setManager:(Employee*)newManager {
  NSLog(@"Meet the new boss...");
  _manager = newManager;
}

@end

int main(int argc, char *argv[])
{
  Employee* bob = [[Employee alloc] initWithName:@"Bob"];
  Employee* sue = [[Employee alloc] initWithName:@"Sue"];
  bob.manager = sue;
  [bob report];
  sue = nil;
  [bob report];
  return 0;
}
```

This prints:

```
Meet the new boss...
I work for Sue
I work for (null)
```

Note that the custom setter was only invoked once.
