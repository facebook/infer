This issue is similar to [`PULSE_UNINITIALIZED_VALUE`](#pulse_uninitialized_value), but it is to detect the uninitialized abstract const value in Hack.

For example, in the following code, the `FIELD` can be read by the static method `get_field`.

* It is problematic invoking `static::FIELD`, since it may be resolved to a `A::FIELD` access, if called from `A::get_field()`. Because `FIELD` is abstract in `A`, it is never assigned a value and the vm will crash. Unfortunately, Hack's type system cannot catch this.
* In the `B` class, `FIELD` is initialized, thus invoking `B::get_field` is safe.

```hack
abstract class A {
  abstract const string FIELD;
  
  public static function get_field(): string {
    return static::FIELD;
  }
}

function call_get_field_bad(): string {
  return A::get_field();
}

class B extends A {
  const string FIELD = "defined";
}

function call_get_field_ok(): string {
  return B::get_field();
}
```
