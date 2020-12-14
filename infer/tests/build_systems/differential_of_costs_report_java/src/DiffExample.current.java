/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import com.google.common.base.Preconditions;
import java.util.ArrayList;

// This class has the following costs:
// constructor: constant
// private access$000 method for accessing f8
// f1: top
// f2: 0
// f3: constant
// f4: linear
// f5: n log n
// f6: n log n
// f7: top by call to f1
// f8: unreachable
// f9: quadratic

public class DiffExample {

  int z;
  ArrayList<String> list =
      new ArrayList<String>() {
      
        @Override
        public String toString() {
           f9(z);
          return super.toString();
        }
      };
    // cost: top
    private static void f1(int k) {
        int i = 0;
        while (i >=0) {
            i++;
        }
    }

  // cost: constant (0)
  private static void f2(int k) {}

    // cost: constant (5)
    private static int f3() {
        int i, j;
        i = 17;
        j = 31;

        return i + j + 3 + 7;
    }

    // cost: linear
    private static int f4(int k) {
        for (int i = 0; i < k; i++) {
        }
        return 0;
    }

    // cost: n log n
    private void f5(ArrayList<Integer> list) {
      java.util.Collections.sort(list);
    }

    // cost: n log n
    private void f6(ArrayList<Integer> list) {
      f5(list);
    }

    // cost: top by call to f1
    private static void f7(int k) {
        f1(k);
    }


   private void f8() {
       int x = 0;
       Preconditions.checkArgument(false);       
   }

   // cost: quadratic
    private static void f9(int k) {
        for (int i = 0; i < k; i++) {
            f4(k);
        }
    }

  void f10(int x) {
    f4(x); // don't report here since previous version just throw exception
  }
}
