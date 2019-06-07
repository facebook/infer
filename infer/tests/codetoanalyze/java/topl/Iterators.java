/*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.util.*;

class Iterators {
  void hasNextBad_FN(List<Integer> x) {
    Iterator<Integer> i = x.iterator();
    i.next();
  }
}
