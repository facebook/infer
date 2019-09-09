/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import android.support.v4.app.Fragment;
import com.facebook.infer.annotation.Cleanup;
import javax.annotation.Nullable;

/**
 * It is common in Android code to recycle objects (e.g. views) by nullifying them in the "cleanup"
 * methods that are called after object lifecycle is over. This allows the GC to recycle without
 * waiting for the outer object to be freed. This is safe because these fields are not going to be
 * accessed after cleanup. So it is not necessary to annotate those fields with @Nullable.
 */
class CanAssignNullInCleanupMethods extends Fragment {

  String someObject = "";

  @Override
  public void onDestroyView() {
    // onDestroyView is a special method: OK to nullify here
    someObject = null;
  }

  @Override
  public void onDestroy() {
    // onDestroy is a special method: OK to nullify here
    someObject = null;
  }

  @Cleanup
  public void assignNullInCleanupMethodIsOK() {
    // The method is marked as cleanup.
    // OK to nullify here.
    someObject = null;
  }

  public void assignNullInAnyOtherMethodIsBAD() {
    someObject = null; // BAD: field is not nullable
  }
}

public class FieldNotNullable {
  @Nullable String nullable = "";
  String notNullable = "";

  String initializeNonNullableWithNullIsBAD = null;
  @Nullable String initializeNullableWithNullIsOK = null;

  @Nullable
  String getNullable() {
    return "";
  }

  String getNotNullable() {
    return "";
  }

  void setNullableToNotNullableIsBAD(@Nullable String s) {
    notNullable = null; // BAD
    notNullable = s; // BAD
    notNullable = getNullable(); // BAD (even though getNullable() does not really return null)
  }

  void setNullableToNullableIsOK(@Nullable String s) {
    nullable = null; // OK
    nullable = s; // OK
    nullable = getNullable(); // OK
  }

  void setNotNullableToNotNullableIsOK(String s) {
    notNullable = "abc"; // OK
    notNullable = s; // OK
    notNullable = getNotNullable(); // OK
  }
}

