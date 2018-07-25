/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import java.util.List;
import java.util.ArrayList;

public class ArrayListTest {

  public void iterate_over_arraylist(ArrayList<Integer> list) {
    for (int i = 0, size = list.size(); i < size; ++i) {}
  }

   public void iterate_over_local_arraylist(ArrayList<Integer> list) {
     ArrayList<Integer> local_list = list;
     for (int i = 0, size = local_list.size(); i < size; ++i) {}
  }

  public void arraylist_empty_underrun_bad() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(-1, 42);
  }
  public void arraylist_empty_ok() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(0, 42);
  }
  public void arraylist_empty_overrun_bad() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(1, 42);
  }

  public void arraylist_add3_overrun_bad() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(42);
    list.add(1337);
    list.add(1984);
    list.add(4, 666);
  }


  // we can't set the size of the list to 10 because it depends on how
  // many times the loop is executed.Should be fixed once we have
  // relational domain working.
   public void arraylist_add_in_loop_FP() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    for (int i = 0; i < 10; ++i) {
      list.add(i);
    }
      for (int i = 0, size = list.size(); i < size; ++i) {}

   }

  public void arraylist_add_in_loop_ok() {
    ArrayList<Integer> list = new ArrayList<Integer>();
      list.add(0);
      list.add(1);
      list.add(2);
      list.add(3);
      list.add(4);
      list.add(5);
      list.add(6);
      list.add(7);
      list.add(8);
      list.add(9);
      list.add(10);
      list.add(11);
      list.add(12);
      list.add(13);
      list.add(14);
      list.add(15);

      for (int i = 0, size = list.size(); i < size; ++i) {}


  }


   public void arraylist_addAll_bad() {
    ArrayList<Integer> list = new ArrayList<Integer>();
      list.add(2);
      list.add(3);

      ArrayList<Integer> list2 = new ArrayList<Integer>();
      list2.add(0);
      list2.add(1);

      list2.addAll(0,list);
      list2.addAll(5,list);

  }

}
