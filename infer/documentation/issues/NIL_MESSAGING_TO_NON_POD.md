In Objective-C, calling a method on `nil` (or in Objective-C terms, sending a message to `nil`) does not crash,
it simply returns a falsy value (nil/0/false). However, sending a message that returns
a non-POD C++ type (POD being ["Plain Old Data"](https://en.cppreference.com/w/cpp/named_req/PODType), essentially
anything that cannot be compiled as a C-style struct) to `nil` causes undefined behaviour.

```objectivec
std::shared_ptr<int> callMethodReturnsnonPOD() {
  SomeObject* obj = getObjectOrNil();
  std::shared_ptr<int> d = [obj returnsnonPOD]; // UB
  return d;
}
```

To fix the above issue, we need to check if `obj` is
not `nil` before calling the `returnsnonPOD` method:

```objectivec
std::shared_ptr<int> callMethodReturnsnonPOD(bool b) {
  SomeObject* obj = getObjectOrNil(b);
  if (obj == nil) { return std::make_shared<int>(0); }
  std::shared_ptr<int> d = [obj returnsnonPOD];
  return d;
}
```
