/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

namespace goto_out_of_scope {

struct C {
  C(int v) : f(v) {}
  ~C();
  int f;
};

int goto_out_of_scope_bad(int n) {
  C* pc;
  {
    C c(3);
    pc = &c;
    if (n > 0) {
      goto out;
    }
  }
  return 0;
out:
  return pc->f;
}

int goto_out_of_nested_scopes_bad(int n) {
  C* pc;
  {
    C c1(1);
    pc = &c1;
    {
      C c2(2);
      if (n > 0) {
        goto out;
      }
    }
  }
  return 0;
out:
  return pc->f;
}

int goto_backward_past_declaration_bad(int n) {
  C* pc = nullptr;
again:
  if (pc != nullptr) {
    return pc->f;
  }
  C c(n);
  pc = &c;
  goto again;
}

int goto_within_scope_ok(int n) {
  C c(3);
  C* pc = &c;
  if (n > 0) {
    goto out;
  }
  n++;
out:
  return pc->f;
}

int goto_into_nested_scope_ok(int n) {
  C c(3);
  C* pc = &c;
  if (n > 0) {
    goto in;
  }
  n++;
  {
  in:
    n += pc->f;
  }
  return n;
}

struct Owner {
  int* p;
  Owner() : p(new int(0)) {}
  ~Owner() { delete p; }
};

int goto_out_of_owner_scope_ok(int n) {
  {
    Owner o;
    if (n > 0) {
      goto out;
    }
  }
  return 0;
out:
  return 1;
}

struct OwnerHolder {
  Owner o;
};

int goto_out_of_conditional_extended_temporary_ok(bool b, int n) {
  {
    // the lifetime of the temporary created in either branch is extended to
    // that of r
    const Owner& r = b ? OwnerHolder().o : OwnerHolder().o;
    if (n > 0) {
      goto out;
    }
  }
  return 0;
out:
  return 1;
}

int init_capture_stmt_expr_ok() {
  auto f = [v = ({
              Owner o;
              *o.p;
            })] { return v; };
  return f();
}

int goto_in_init_capture_ok(int n) {
  auto f = [v = ({
              int r = 0;
              {
                Owner o;
                if (n > 0) {
                  goto out;
                }
                r = *o.p;
              }
            out:;
              r;
            })] { return v; };
  return f();
}

int goto_out_of_unique_ptr_scope_ok(int n) {
  {
    std::unique_ptr<int> p(new int(3));
    if (n > 0) {
      goto out;
    }
  }
  return 0;
out:
  return 1;
}

} // namespace goto_out_of_scope
