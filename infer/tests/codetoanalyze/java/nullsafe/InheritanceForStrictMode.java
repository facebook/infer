/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/** Test for checking how @NullsafeStrict mode plays with inheritance rule */
package codetoanalyze.java.nullsafe;

import com.facebook.infer.annotation.NullsafeStrict;
import javax.annotation.Nullable;

public class InheritanceForStrictMode {
  class NonStrictBase {
    public @Nullable String okToRemoveNullableInChildren() {
      return null;
    }

    public String badToAddNullableInChildren() {
      return "";
    }

    public void params(
        @Nullable String badToRemoveNullableInChildren, String okToAddNullableInChildren) {}
  }

  // Exactly as NonStrictBase, except that it is marked as @NullsafeStrict
  @NullsafeStrict
  class StrictBase {
    public @Nullable String okToRemoveNullableInChildren() {
      return null;
    }

    public String badToAddNullableInChildren() {
      return "";
    }

    public void params(
        @Nullable String badToRemoveNullableInChildren, String okToAddNullableInChildren) {}
  }

  // Expecting all issues to be surfaced as ERRORs
  // NOTE: we currently DON'T require the base to be strictified in order to strictify a child (see
  // T60513926)
  @NullsafeStrict
  class StrictExtendingNonstrict extends NonStrictBase {
    public @Override String okToRemoveNullableInChildren() {
      return "";
    }

    public @Override @Nullable String badToAddNullableInChildren() {
      return null;
    }

    public @Override void params(
        String badToRemoveNullableInChildren, @Nullable String okToAddNullableInChildren) {}
  }

  // Expecting all issues to be surfaced as ERRORs
  @NullsafeStrict
  class StrictExtendingStrict extends StrictBase {
    public @Override String okToRemoveNullableInChildren() {
      return "";
    }

    public @Override @Nullable String badToAddNullableInChildren() {
      return null;
    }

    public @Override void params(
        String badToRemoveNullableInChildren, @Nullable String okToAddNullableInChildren) {}
  }

  // Expecting all issues to be surfaces as WARNINGs (even that we extend a strict class)
  class NonStrictExtendingStrict extends StrictBase {
    public @Override String okToRemoveNullableInChildren() {
      return "";
    }

    public @Override @Nullable String badToAddNullableInChildren() {
      return null;
    }

    public @Override void params(
        String badToRemoveNullableInChildren, @Nullable String okToAddNullableInChildren) {}
  }
}
