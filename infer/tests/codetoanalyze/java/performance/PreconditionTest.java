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
  public void constant_FP(ArrayList<Integer> list) {
    Preconditions.checkArgument(list.size() == 2);
    for (int i = 0; i < list.size(); i++) {}
  }
}
