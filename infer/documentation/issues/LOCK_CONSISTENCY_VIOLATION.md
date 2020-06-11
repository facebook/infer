This is a C++ and Objective C error reported whenever:

- A class contains a member `lock` used for synchronization (most often a
  `std::mutex`).
- It has a public method which writes to some member `x` while holding `lock`.
- It has a public method which reads `x` without holding `lock`.

The above may happen through a chain of calls. Above, `x` may also be a
container (an array, a vector, etc).

### Fixing Lock Consistency Violation reports

- Avoid the offending access (most often the read). Of course, this may not be
  possible.
- Use synchronization to protect the read, by using the same lock protecting the
  corresponding write.
- Make the method doing the read access private. This should silence the
  warning, since Infer looks for a pair of non-private methods. Objective-C:
  Infer considers a method as private if it's not exported in the header-file
  interface.
