/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import com.google.common.base.Preconditions;
import java.util.ArrayList;

class PreconditionTest {
  // should be constant
  public void checkArgument_constant(ArrayList<Integer> list) {
    Preconditions.checkArgument(list.size() == 2);
    for (int i = 0; i < list.size(); i++) {}
  }

  // should be constant
  public void checkState_constant(ArrayList<Integer> list) {
    Preconditions.checkState(list.size() == 2);
    for (int i = 0; i < list.size(); i++) {}
  }

  public class Constant {
    public static final int FIVE = 5;
  }

  // should be constant
  static void constant_array(int[] a) {
    Preconditions.checkArgument(a.length == Constant.FIVE);
    for (int i : a) {
      Preconditions.checkArgument(i >= 0);
    }
  }

  public void checkNotNull_linear(ArrayList<Integer> list, Object o) {
    ArrayList<Integer> mediaList = Preconditions.checkNotNull(list, o);
    for (Integer el : mediaList) {}
  }
}
