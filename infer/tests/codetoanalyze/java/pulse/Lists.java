/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import java.util.*;

class Lists {

  void emptyRemembersOk(List l) {
    boolean empty = l.isEmpty();
    Object o = null;
    if (empty != l.isEmpty()) {
      o.toString();
    }
  }

  void FN_latent_removeInvalidatesNonEmptinessNPEBad(List l) {
    l.removeAll(l);
    Object o = null;
    if (l.isEmpty()) {
      o.toString();
    }
  }

  void clearCausesEmptinessNPEBad(List l) {
    l.clear();
    Object o = null;
    if (l.isEmpty()) {
      o.toString();
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

  void getElementNPEBad() {
    List l = new ArrayList();
    if (!l.isEmpty()) {
      return;
    }
    getElement(l).toString();
  }

  void getElementOk() {
    List l = new ArrayList();
    if (!l.isEmpty()) {
      Object o = null;
      o.toString();
    }
  }

  void addInvalidatesEmptinessNPEBad(List l) {
    l.add(0, new Object());
    Object o = null;
    if (!l.isEmpty()) {
      o.toString();
    }
  }

  void addAndRemoveEmptinessNPEBad() {
    List l = new ArrayList();
    Integer i = new Integer(1);
    l.add(i);
    l.remove(i);
    if (l.isEmpty()) {
      getElement(l).toString();
    }
  }

  void FP_removeOnEmptyListOk() {
    List l = new LinkedList();
    Object removed = l.remove(0);
    if (removed != null) {
      getElement(l).toString();
    }
  }

  void removeElementBad() {
    List l = new LinkedList();
    Integer i = new Integer(4);
    l.add(i);
    boolean removed = l.remove(i);
    if (removed) {
      Object o = null;
      o.toString();
    }
  }
}
