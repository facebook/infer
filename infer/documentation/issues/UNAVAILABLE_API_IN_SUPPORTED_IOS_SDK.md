This checks warns you when you are using an API (constant, method call, etc.)
that is only defined in a version higher than the version that you support. To
enable this check, pass to Infer the option
`--iphoneos-target-sdk-version version`. Calling an undefined API will lead to a
crash in the app. To fix this, you can choose a different API or use it inside
an if, as in:

```objectivec
if ([UIFont respondsToSelector:@selector(systemFontOfSize:weight:)]) {
  font = [UIFont systemFontOfSize:size weight:0];
}
```

or

```objectivec
if (kCFCoreFoundationVersionNumber >= kCFCoreFoundationVersionNumber_iOS_9_0) {
  font = [UIFont systemFontOfSize:size weight:0];
}
```
