/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct point {
  int x;
  int y;
};

int get_x(struct point* p) { return p->x; }

void set_y(struct point* p, int v) { p->y = v; }
