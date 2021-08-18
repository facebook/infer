/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe;;

import java.util.Objects;
import javax.annotation.Nullable;

public class NullLibraries {

  void objectsNonNullOK(@Nullable Object object) {
    if (Objects.nonNull(object)) {
      object.toString();
    }
  }

  void objectsNonNullWarn(@Nullable Object object) {
    if (! Objects.nonNull(object)) {
      object.toString();
    }
  }

  void objectsIsNullWarn(@Nullable Object object) {
    if (Objects.isNull(object)) {
      object.toString();
    }
  }

  void objectsIsNullOK(@Nullable Object object) {
    if (!Objects.isNull(object)) {
      object.toString();
    }
  }

  void objectsRequireNonNullOK(@Nullable Object object) {
    Objects.requireNonNull(object);
    object.toString();
  }

}
