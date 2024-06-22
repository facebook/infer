This issue is similar to [`PULSE_UNINITIALIZED_VALUE`](#pulse_uninitialized_value), but it is to warn
reading a missing key of dictionary in Hack.

For example, in the following code, the dictionary `$d` has no entry for `bye`, so reading
`$d['bye']` will throw the `OutOfBoundsException` exception, which is usually unexpected from
developers.  We can use a safer function `idx` instead when keys of a dictionary is unclear.

```hack
function simple_bad() : int {
  $d = dict['hi' => 42, 'hello' => 52];
  return $d['bye'];
}
```
