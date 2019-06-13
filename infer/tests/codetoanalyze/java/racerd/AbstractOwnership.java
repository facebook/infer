/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import com.facebook.infer.annotation.ReturnsOwnership;
import javax.annotation.concurrent.ThreadSafe;

// no races should be reported here
// abstract getThis should get a default summary returning conditional ownership

@ThreadSafe
abstract class Component {
  abstract static class Builder<T extends Builder<T>> {
    abstract T getThis();

    private int i;

    public T set(int i) {
      this.i = i;
      return getThis();
    }

    public T background() {
      return getThis();
    }

    @ReturnsOwnership
    abstract Component build();
  }
}

@ThreadSafe
class Column extends Component {
  static Component onCreateLayoutOk() {
    Component.Builder<?> builder = ColumnBuilder.create().background();
    return builder.set(0).build();
  }

  static class ColumnBuilder extends Component.Builder<ColumnBuilder> {
    static ColumnBuilder create() {
      return new ColumnBuilder();
    }

    @Override
    ColumnBuilder getThis() {
      return this;
    }

    @Override
    Column build() {
      return new Column();
    }
  }
}
