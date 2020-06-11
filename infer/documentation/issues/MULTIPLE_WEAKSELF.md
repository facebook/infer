An Objective-C block uses `weakSelf` more than once. This could lead to
unexpected behaviour. Even if `weakSelf` is not nil in the first use, it could
be nil in the following uses since the object that `weakSelf` points to could be
freed anytime. One should assign it to a strong pointer first, and then use it
in the block.
