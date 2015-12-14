/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.eradicate;

import java.lang.ref.PhantomReference;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.lang.ref.WeakReference;
import java.util.concurrent.atomic.AtomicReference;

public class LibraryCalls {

  String badReferenceDereference(Reference ref) {
    return ref.get().toString();
  }

  String badWeakReferenceDereference(WeakReference ref) {
    return ref.get().toString();
  }

  String badPhantomReferenceDereference(PhantomReference ref) {
    return ref.get().toString();
  }

  String badSoftReferenceDereference(SoftReference ref) {
    return ref.get().toString();
  }

  String badAtomicReferenceDereference(AtomicReference ref) {
    return ref.get().toString();
  }

}
