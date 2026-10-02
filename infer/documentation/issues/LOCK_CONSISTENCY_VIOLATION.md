This is an error reported on C++ and Objective C classes whenever:

- A public method writes to some member `x` while holding a lock, taken directly
  or by a callee.
- A public method reads `x` without holding a lock.

The above may happen through a chain of calls. Above, `x` may also be a
container (an array, a vector, etc). Methods and lambdas that are started as
threads in the same file (with `std::thread` or `std::jthread`, also through
`emplace_back` on a container of threads, a non-deferred `std::async` or
`pthread_create`) count as public methods.

### Fixing Lock Consistency Violation reports

- Avoid the offending access (most often the read). Of course, this may not be
  possible.
- Use synchronization to protect the read, by using the same lock protecting the
  corresponding write.
- Make the method doing the read access private. This should silence the
  warning, since Infer looks for a pair of non-private methods, unless the
  method is started as a thread. Objective-C: Infer considers a method as
  private if it's not exported in the header-file interface.
