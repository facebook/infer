/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe;

import com.facebook.infer.annotation.Nullsafe;

/** Test to ensure we correctly evaluate mode for nested classes */
@Nullsafe(Nullsafe.Mode.LOCAL)
class NullsafeLocal {

  public String shouldBeNullsafeModeError() {
    return null;
  }

  // Mode should be inherited from the parent class
  class Nested {
    public String shouldBeNullsafeModeError() {
      return null;
    }

    // Mode propagation should be transitive
    class DeeplyNested {
      public String shouldBeNullsafeModeError() {
        return null;
      }
    }

    // This is Local, but not Strict mode
    public String returningDefaultNotNullIsOK() {
      return Default.getString();
    }
  }

  // It is OK to make nested classes more strict
  @Nullsafe(Nullsafe.Mode.STRICT)
  class NestedStrict {
    public String returningDefaultNotNullIsError() {
      return Default.getString();
    }
  }

  // No need to repeat the mode - it is redundant
  @Nullsafe(Nullsafe.Mode.LOCAL)
  class NestedExplicitLocal {
    public String shouldBeNullsafeModeError() {
      return null;
    }
  }
}

@Nullsafe(Nullsafe.Mode.STRICT)
class NullsafeStrict {
  public String returningDefaultNotNullIsError() {
    return Default.getString();
  }

  // STRICT mode is propagated to the nested class
  class Nested {
    public String returningDefaultNotNullIsError() {
      return Default.getString();
    }

    // Impossible to downgrade the level of nested class, even if the nested mode
    // is implicit
    @Nullsafe(Nullsafe.Mode.LOCAL)
    class DeeplyNestedLocalIsStillStrict {
      public String returningDefaultNotNullIsError() {
        return Default.getString();
      }
    }
  }

  // Impossible to downgrade the level of nested class
  @Nullsafe(Nullsafe.Mode.LOCAL)
  class NestedLocalIsStillStrict {
    public String returningDefaultNotNullIsError() {
      return Default.getString();
    }
  }
}

class Default {
  public static String getString() {
    return "";
  }

  // OK for nested to be @Nullsafe but the outer is not
  @Nullsafe(Nullsafe.Mode.LOCAL)
  class NestedLocal {
    public String shouldBeNullsafeModeError() {
      return null;
    }

    // This is Local, but not Strict mode
    public String returningDefaultNotNullIsOK() {
      return Default.getString();
    }

    // And we can increase strictness even more
    @Nullsafe(Nullsafe.Mode.STRICT)
    class DeeplyNestedStrict {
      public String returningDefaultNotNullIsError() {
        return Default.getString();
      }
    }
  }
}

class A {
  public static String getString() {
    return "";
  }
}

class B {}

class C {
  public static String getString() {
    return "";
  }
}

@Nullsafe(value = Nullsafe.Mode.LOCAL, trustOnly = @Nullsafe.TrustList({A.class, B.class}))
class TrustSome {

  public String trustA_OK() {
    return A.getString();
  }

  public String dontTrustC_Bad() {
    return C.getString();
  }

  // Inherits mode from the outer, the same trust
  class NotAnnotatedNested {
    public String trustA_OK() {
      return A.getString();
    }

    public String dontTrustC_Bad() {
      return C.getString();
    }
  }

  // This class does not trust A anymore
  @Nullsafe(value = Nullsafe.Mode.LOCAL, trustOnly = @Nullsafe.TrustList({B.class}))
  class CanRemoveFromTrustList {
    public String dontTrustA_BAD() {
      return A.getString();
    }
  }

  // Lousy attempt to add a class C to trust list
  // Should have a special issue suggesting to remove C from the list.
  @Nullsafe(value = Nullsafe.Mode.LOCAL, trustOnly = @Nullsafe.TrustList({A.class, C.class}))
  class CanNotAddToTrustList {
    public String stillDontTrustC_BAD() {
      return C.getString();
    }
  }
}
