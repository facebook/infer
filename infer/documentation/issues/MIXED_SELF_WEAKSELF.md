This happens when an Objective-C block captures both `self` and `weakSelf`, a
weak pointer to `self`. Possibly the developer meant to capture only `weakSelf`
to avoid a retain cycle, but made a typo and used `self` as well in the block,
instead of `strongSelf`. In this case, this could cause a retain cycle.
