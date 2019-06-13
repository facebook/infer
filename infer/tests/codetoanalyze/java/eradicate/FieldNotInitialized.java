/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.eradicate;

import android.support.annotation.NonNull;
import android.widget.EditText;
import com.facebook.infer.annotation.SuppressViewNullability;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.inject.Inject;

// for butterknife
@interface Bind {}

public class FieldNotInitialized {

  String a;

  @Nullable String b;

  @Nonnull String c; // Means: assume it will be initialized to a nonnull value somewhere else.

  @Inject String d; // Means: assume it will be initialized via dependency injection

  @NonNull String e;

  @Bind EditText f; // Means: assume it will be initialized, and ignore null assignment

  @SuppressViewNullability EditText g;

  //  Eradicate should only report one initialization error
  FieldNotInitialized() {}

  void testNullifyFields() {
    f = null; // OK  the framework could write null into the field
    g = null; // OK  the framework could write null into the field
  }

  class OnlyRead {
    Object o;

    OnlyRead() {
      Object x = o; // not initialized
    }
  }

  class WriteItself {
    Object o;

    WriteItself() {
      o = o; // not initialized
    }
  }

  class Swap {
    Object o1;
    Object o2;

    Swap() {
      o1 = o2; // not initialized
      o2 = new Object();
    }
  }

  class SwapOK {
    Object o1;
    Object o2;

    SwapOK() {
      o1 = new Object();
      o2 = o1;
    }
  }

  class OnlyReadIndirect {
    Object o1;
    Object o2;

    private void indirect() {
      Object x = o1; // not initialized
      o2 = new Object();
    }

    OnlyReadIndirect() {
      indirect();
    }
  }

  class ConditionalFieldInit {
    Object o1;
    @Nullable Object o2 = null;

    public ConditionalFieldInit() {
      if (o2 != null) {
        o1 = new Object(); // Not always initialized
      }
    }
  }

  class InitIfNull {
    Object o;

    public InitIfNull() {
      if (o == null) o = new Object();
    }
  }

  class InitIfNull2 {
    Object o;

    public InitIfNull2(Object x) {
      if (o == null) o = x;
    }
  }

  class InitIfNull3 {
    Object o;

    Object getNotNull() {
      return new Object();
    }

    public InitIfNull3() {
      if (o == null) o = getNotNull();
    }
  }

  class InitCircular {
    String s;

    InitCircular() {
      String tmp = s;
      s = tmp; // s is not initialized: circular initialization
    }
  }

  class InitWithOtherClass {
    class OtherClass {
      String s = "";
    }

    String s;

    InitWithOtherClass(OtherClass x) {
      s = x.s;
    }
  }

  class InitWithThisClass {

    String s;

    InitWithThisClass(InitWithThisClass x) {
      s = x.s;
    }
  }
}
