/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int local_labels_with_same_name(int c) {
  int x = 0;
  {
    __label__ end;
    if (c)
      goto end;
    x = 1;
  end:;
  }
  {
    __label__ end;
    if (c)
      goto end;
    x = 2;
  end:;
  }
  return x;
}

int local_label_shadows_function_label(int c) {
  int x = 0;
  {
    __label__ end;
    if (c)
      goto end;
    x = 1;
  end:;
  }
  if (c)
    goto end;
  x = 2;
end:
  return x;
}

int local_label_in_statement_expression(int a) {
  int v = ({
    __label__ here;
  here:
    a + 1;
  });
  return v;
}
