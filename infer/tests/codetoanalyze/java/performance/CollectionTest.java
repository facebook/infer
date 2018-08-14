/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import java.util.Collection;
import java.util.Iterator;
import java.util.concurrent.ConcurrentLinkedQueue;

public class CollectionTest {

  interface MyCollection<E> extends Collection<E> {}

  void iterate_over_mycollection(MyCollection<Integer> list) {
    for (int i = 0, size = list.size(); i < size; ++i) {}
  }

  void iterate_over_some_java_collection(
      ConcurrentLinkedQueue<MyCollection<Integer>> mSubscribers) {
    for (MyCollection<Integer> list : mSubscribers) {
    }
  }

  // Expected |mSubscribers| * |list| but we get T
  // because we are not tracking elements of collections
  void iterate_over_mycollection_quad_FP(
      ConcurrentLinkedQueue<MyCollection<Integer>> mSubscribers) {
    for (MyCollection<Integer> list : mSubscribers) {
      iterate_over_mycollection(list);
    }
  }
}
