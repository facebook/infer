Infer reports these warnings in Objective-C when a boxed primitive type such as
`NSNumber *` is coerced to a boolean in a comparison. For example, consider the
code

```objectivec
void foo(NSNumber * n) {
  if (n) ...
```

The branch in the above code will be taken when the pointer `n` is non-`nil`,
but the programmer might have actually wanted the branch to be taken when the
integer pointed to by `n` is nonzero (e.g., she may have meant to call an
accessor like `[n intValue]` instead). Infer will ask the programmer explicitly
compare `n` to `nil` or call an accessor to clarify her intention.
