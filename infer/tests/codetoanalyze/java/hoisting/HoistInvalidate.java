/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import com.sun.source.tree.Tree;
import com.sun.tools.javac.util.List;

class HoistInvalidate<T extends Tree> {

  // item will be invalidated
  void loop_over_sun_list_dont_hoist(List<T> list) {
    for (List<T> item = list; item.nonEmpty(); item = item.tail) {}
  }
}
