/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import java.util.List;

class Lists {

  void emptyRemembersOk(List l) {
    boolean empty = l.isEmpty();
    Object o = null;
    if (empty != l.isEmpty()) {
      o.toString();
    }
  }

  void removeInvalidatesNonEmptinessNPE(List l, int i) {
    if (!l.isEmpty()) {
      l.remove(i);
      Object o = null;
      if (l.isEmpty()) {
        o.toString();
      }
    }
  }

  void clearCausesEmptinessNPE(List l, int i) {
    if (!l.isEmpty()) {
      l.clear();
      Object o = null;
      if (l.isEmpty()) {
        o.toString();
      }
    }
  }

  // it would be too noisy to report here
  void plainGetOk(List l, int i) {
    l.get(i).toString();
  }

  Object getElement(List l) {
    return l.isEmpty() ? null : l.get(0);
  }

  void getElementOk(List l) {
    if (l.isEmpty()) {
      return;
    }
    getElement(l).toString();
  }

  void getElementNPE(List l) {
    if (!l.isEmpty()) {
      return;
    }
    getElement(l).toString();
  }

  // don't fully understand why we don't get this one; model should allow it
  void FN_addInvalidatesEmptinessNPE(List l) {
    if (l.isEmpty()) {
      l.add(0, new Object());
      Object o = null;
      if (!l.isEmpty()) {
        o.toString();
      }
    }
  }
}
