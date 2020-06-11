This check warns you when you are accessing an atomic property directly with an
ivar. This makes the atomic property not atomic anymore. So potentially you may
get a race condition.

To fix the problem you need to access properties with their getter or setter.
