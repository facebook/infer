/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.facebook.litho;

public class Column extends Component {

  static native Builder acquire();

  public static Builder create() {
    Builder builder = acquire();
    if (builder == null) {
      builder = new Builder();
    }
    return builder;
  }

  public static class Builder extends Component.Builder {

    public Builder child(Component child) {
      if (child == null) {
        return this;
      }
      return this;
    }

    public Builder child(Component.Builder child) {
      if (child == null) {
        return this;
      }
      return child(child.build());
    }

    Column mColumn;

    @Override
    public Column build() {
      return mColumn;
    }
  }
}
