/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import javax.annotation.concurrent.ThreadSafe;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.MyImmutableList;
import com.google.common.collect.ImmutableList.Builder;

@ThreadSafe
public class Builders {

  static class Obj {
    final String f;
    String g;

    public Obj(String f, String g) {
      this.f = f;
      this.g = g;
    }

    public static class Builder {
      String f;
      String g;

      public Builder setFromObj(Obj input) {
        this.f = input.f;
        this.g = input.g;
        return this;
      }

      public Obj build() {
        return new Obj(f, g);
      }

      public Builder setF(String f) {
        this.f = f;
        return this;
      }

      public Builder setG(String g) {
        this.g = g;
        return this;
      }
    }
  }

  public void guavaBuilderOk(ImmutableList.Builder<String> builder) {
    builder.add("foo");
    builder.build();
  }

  public Obj customBuilderOk1(Obj.Builder builder) {
    builder.setF("f");
    builder.setG("g");
    return builder.build();
  }

  public Obj customBuilderOk2(Obj.Builder builder) {
    return builder.setF("f").setG("g").build();
  }

  public Obj customBuilderOk3(Obj input) {
    Obj.Builder builder = new Obj.Builder();
    return builder.setFromObj(input).build();
  }

  public void writeImmutableListFieldOk(MyImmutableList<Object> list) {
    list.writeFld();
  }

  public Obj mutateBad(Obj o) {
    o.g = "";
    return o;
  }

  public Obj buildThenMutateBad(Obj input) {
    Obj.Builder builder = new Obj.Builder();
    Obj output = builder.setFromObj(input).build();
    input.g = "";
    return output;
  }

}

@ThreadSafe
class TopLevelBuilder {
  public String g;

  public void setG(String g) {
    this.g = g; // still want to warn if the builder is annotated ThreadSafe
  }

}
