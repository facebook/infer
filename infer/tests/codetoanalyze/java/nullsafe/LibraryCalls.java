/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

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
