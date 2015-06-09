/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package codetoanalyze.java.checkers;

import com.google.common.collect.ImmutableList;

import java.util.ArrayList;
import java.util.List;

public class ImmutableCast {

  ImmutableList<String> immutableList = ImmutableList.of("a", "b", "c");

  List<String> badCastFromField() {
    return immutableList;
  }

  List<String> badCast(ImmutableList<String> list) {
    return list;
  }

  List<String> goodCast(ImmutableList<String> list) {
    return new ArrayList<String>(list);
  }

}
