/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.io.*;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Queue;

class PurityModeled {

  double math_random_impure() {
    return Math.random();
  }

  double math_random_infeasible_pure(int x) {
    if (x > 1 && x < 2) {
      return Math.random(); // this path will never be taken
    }
    return 0;
  }

  void arraycopy_pure(int[] src) {
    int[] dst = {5, 10, 20, 30, 40, 50};
    // copies an array from the specified source array
    System.arraycopy(src, 0, dst, 0, 1);
  }

  public void array_length_loop_pure(Integer[] array) {
    for (int i = 0; i < array.length; i++) {}
  }

  void FN_write_impure() {
    byte[] temp = new byte[4];
    System.out.write(temp, 0, 4);
  }

  void FN_call_write_impure() {
    FN_write_impure();
  }

  int math_random_in_loop_impure(int x) {
    int p = 0;
    for (int i = 0; i < x; i++) {
      p += Math.random();
      FN_call_write_impure();
    }

    return p;
  }

  void list_size_pure(ArrayList<String> list) {
    for (int i = 0; i < list.size(); i++) {}
  }

  void list_add_impure(ArrayList<String> list) {
    list.add("a");
  }

  void list_addall_impure(ArrayList<String> list1, ArrayList<String> list2) {
    list1.addAll(list2);
  }

  void enum_loop_pure(Enumeration<String> e) {

    for (; e.hasMoreElements(); ) {
      Object o = e.nextElement();
    }
  }

  void remove_impure(Iterator<String> i) {
    while (i.hasNext()) {
      if (i.next().equals("Orange")) {
        i.remove();
        break;
      }
    }
  }

  void list_set_impure(ArrayList<String> list) {
    list.set(0, "e");
  }

  void call_set_impure(ArrayList<String> list) {
    list_set_impure(list);
    list_set_impure(list);
  }

  // Pulse can only widen a fixed number of times, hence it thinks
  // that the exit of the loop never reaches and results in empty
  // post.
  void timing_call_in_loop_impure() {
    for (int i = 0; i < 10; i++) {
      System.nanoTime();
    }
  }

  // Pulse can only widen a fixed number of times, hence it thinks
  // that the exit of the loop never reaches and results in empty
  // post which is considered to be impure.
  void constant_loop_pure_FP() {
    for (int i = 0; i < 10; i++) {}
  }

  // Since n is symbolic, pruning doesn't result in infeasible path,
  // but we assume that the parameter [n] must be 3 due to constant(4)
  // "widening" in pulse.
  void timing_call_in_loop_symb_impure(int n) {
    for (int i = 0; i < n; i++) {
      System.nanoTime();
    }
  }

  // Due to getting the wrong summary for the callee (a=3), Pulse ends
  // up thinking that the parameter [a] must be 3 in the loop. Hence, as
  // a result of pruning, exit node becomes infeasible and we get
  // empty summary which is considered to be impure.
  void call_timing_symb_impure(int a) {
    for (int i = 0; i < a; i++) {
      timing_call_in_loop_symb_impure(a);
    }
  }

  // The relation between the parameter and the argument to the callee
  // is broken. Although, the final pulse summary for this function is
  // still wrong.
  void call_timing_symb_unrelated_impure(int a, int b) {
    for (int i = 0; i < a; i++) {
      timing_call_in_loop_symb_impure(b);
    }
  }

  enum Color {
    RED,
    GREEN,
    BLUE; // values() calls clone
  }

  public void enum_iter_pure() {
    for (Color c : Color.values()) {}
  }

  @SuppressWarnings("unchecked")
  void clone_pure(ArrayList<String> list) {
    ArrayList<String> cloned = (ArrayList<String>) list.clone();
    cloned.add(""); // no change the list
  }

  String replace_impure(String s) {
    return s.replace('a', 'f');
  }

  void process_queue_impure(ArrayList<Integer> list, Queue<Integer> queue) {
    for (Integer el : list) {
      queue.add(el);
    }
  }

  String add_impure(ArrayList<Integer> list) {
    Integer el = list.get(0);
    list.add(4);
    return el.toString();
  }

  void append_impure(StringBuilder strBuilder) {
    strBuilder.append("JavaGuru");
  }

  void append_pure() {
    StringBuilder strBuilder = new StringBuilder("Core");
    strBuilder.append("JavaGuru");
  }

  Integer next_impure(Iterator<Integer> it) {
    return it.next();
  }

  String remove_iterator_impure(Iterator<Integer> listIterator) {
    Integer f = listIterator.next();
    listIterator.remove();
    return f.toString();
  }

  String remove_fresh_impure(ArrayList<Integer> list) {
    Iterator<Integer> listIterator = list.iterator();
    Integer f = listIterator.next();
    listIterator.remove();
    return f.toString();
  }

  void remove_impure_mult(ArrayList<Integer> list) {
    String s1 = remove_fresh_impure(list);
    String s2 = remove_fresh_impure(list);
  }

  public static void remove_all_impure(ArrayList<Integer> list) {
    for (Iterator<Integer> iter = list.iterator(); iter.hasNext(); ) {
      Integer entry = iter.next();
      iter.remove();
      System.out.println(entry.toString());
    }
  }

  void nested_remove_impure(ArrayList<ArrayList<Integer>> list) {
    Iterator<ArrayList<Integer>> listIterator = list.iterator();
    while (listIterator.hasNext()) {
      ArrayList<Integer> inner_list = listIterator.next();
      Iterator<Integer> innerListIterator = inner_list.iterator();
      while (innerListIterator.hasNext()) {
        Integer el = innerListIterator.next();
        innerListIterator.remove();
      }
    }
  }

  void remove_all_directly_impure(ArrayList<Integer> list) {
    for (Integer el : list) {
      list.remove(el); // bad, must remove via iterator.
    }
  }

  public static final String toString_delete_pure(Object args) {
    StringBuilder builder = new StringBuilder(32).append('{');
    if (args != null) {
      if (builder.charAt(builder.length() - 2) == ',') {
        builder.delete(builder.length() - 2, builder.length());
      }
    }
    builder.append('}').setLength(10);
    return builder.toString();
  }

  String getCanonicalPath_pure(File file) throws IOException {
    return file.getCanonicalPath();
  }
}
