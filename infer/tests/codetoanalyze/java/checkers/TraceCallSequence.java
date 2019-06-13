/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

class TraceCallSequence {
  static void begin() {}

  static void end() {}

  static void beginWrapper() {
    begin();
  }

  static void endWrapper() {
    end();
  }

  void thereIsNoEnd() {
    begin();
  } // 1 missing end/stop

  void thereIsNoBeginning() {
    end(); // too many end/stop;
  }

  void ok() {
    begin();
    end();
  }

  void wrapper() {
    begin();
    beginWrapper();
    end();
    endWrapper();
  }

  void exception1(String s) {
    begin();
    int n = s.length();
    end();
  } // 1 missing end/stop

  void exception2(String s) {
    int n = s.length();
    begin();
    end();
  }

  void exception3(String s) {
    begin();
    try {
      int n = s.length();
    } finally {
      end();
    }
  }

  void infinite(int d) {
    int count = 0;
    begin();
    begin();
    while (count < d) {
      end();
      begin();
      count++;
    }
    end();
    end();
  }

  void nondet(int x) {
    if (x > 0) {
      begin();
    } else {
    }
    end(); // too many end/stop
  }

  void grow(int d) {
    int count = 0;
    while (count < d) {
      begin();
    }
  } // 2 missing end/stop

  void testBool(String s) {
    boolean shouldTrace = s.length() == 4;
    if (shouldTrace) {
      begin();
    }

    if (shouldTrace) {
      shouldTrace = false;
    } else {
      shouldTrace = true;
    }

    if (!shouldTrace) {
      end();
    }
  }

  void testBoolLoop1(String s) {
    boolean shouldTrace = true;
    while (s.length() == 4) {
      if (shouldTrace) {
        begin();
        shouldTrace = false;
      } else {
        end();
        shouldTrace = true;
      }
    }
    if (!shouldTrace) {
      end();
    }
  } // 1 missing end/stop

  void testBoolLoop2(String s) {
    boolean shouldTrace = true;
    try {
      while (s.length() == 4) {
        if (shouldTrace) {
          begin();
          shouldTrace = false;
        } else {
          end();
          shouldTrace = true;
        }
      }
    } finally {
      if (!shouldTrace) {
        end();
      }
    }
  }
}
