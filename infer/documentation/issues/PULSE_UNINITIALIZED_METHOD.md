This issue is similar to [`PULSE_UNINITIALIZED_CONST`](#pulse_uninitialized_const), but it is to detect the uninitialized method call in Hack.

For example, in the following code, the static method `foo` is declared only in the interface and the abstract class.  Thus, calling the static method can introduce an unexpected exception or a fatal error, while the type checker does miss the issue.

```hack
interface MyInterface {
  public static function foo(): string;
}

abstract class MyAbstractClass {
  public abstract static function foo(): string;
}

function interface_method_static_method_bad(): string {
  // Uncaught exception 'TypehintViolationException'
  $c = MyInterface::class;
  return $c::foo();
}

function abstract_class_static_method_bad(): string {
  // Fatal error: Cannot call abstract method
  $c = MyAbstractClass::class;
  return $c::foo();
}
```
