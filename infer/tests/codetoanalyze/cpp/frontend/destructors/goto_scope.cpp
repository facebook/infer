/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace goto_scope {

struct X {
  ~X() {}
};

void goto_out_of_nested_scopes(bool b) {
  X x1;
  {
    X x2;
    if (b) {
      X x3;
      goto out;
    }
  }
out:
  X x4;
}

void goto_out_of_loop(bool a, bool b) {
  X x1;
  while (a) {
    X x2;
    if (b)
      goto out;
  }
out:
  return;
}

void goto_backward_past_declaration(bool b) {
  X x1;
again:
  X x2;
  if (b) {
    goto again;
  }
}

void goto_within_scope(bool b) {
  X x1;
  if (b) {
    goto out;
  }
  b = !b;
out:
  X x2;
}

void goto_into_nested_scope(bool b) {
  X x1;
  if (b) {
    goto in;
  }
  {
  in:
    X x2;
  }
}

void goto_at_end_of_scope(bool b) {
  {
    X x1;
    goto out;
  }
out:
  X x2;
}

void goto_and_extended_temporaries(bool b) {
  const X& x1 = X();
  {
    const X& x2 = X();
    if (b) {
      goto out;
    }
  }
out:
  return;
}

template <typename F>
void call(F f) {
  f();
}

// the label in the lambda is not the label of the function
void goto_same_label_in_lambda(bool b) {
  X x1;
out:
  X x2;
  call([]() {
    goto out;
  out:
    return;
  });
  if (b) {
    goto out;
  }
}

// the capture initializers are translated in the enclosing function
void goto_in_lambda_capture(bool b) {
  auto f = [i = ({
              int r = 0;
              {
                X x1;
                if (b) {
                  goto out;
                }
              }
            out:;
              r;
            })]() { return i; };
}

} // namespace goto_scope
