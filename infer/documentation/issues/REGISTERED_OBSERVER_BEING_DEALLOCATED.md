Objects register with a notification center to receive notifications. This check
warns you when an object is registered as observer of a NSNotificationCenter but
it is never unregistered. This is problematic as if the object is not
unregistered the notification center can still send notification even after the
object has been deallocated. In that case we would get a crash.
