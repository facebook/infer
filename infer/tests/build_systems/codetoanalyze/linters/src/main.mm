#include "../../../../codetoanalyze/objcpp/linters/componentkit/FakeComponentKitHeader.h"

@interface SomeClass : CKCompositeComponent
@end
@implementation SomeClass
+ (instancetype) new {
  int i; // error
  return nil;
}
@end
