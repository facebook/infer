/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.quandary;

import com.facebook.infer.builtins.InferTaint;

public class DynamicDispatch {

  static interface Interface {
    public Object returnSource();
    public void callSink(Object o);
    public Object propagate(Object o);
  }

  static class BadInterfaceImpl1 implements Interface {
    @Override
    public Object returnSource() {
      return InferTaint.inferSecretSource();
    }

    @Override
    public void callSink(Object o) {
      InferTaint.inferSensitiveSink(o);
    }

    @Override
    public Object propagate(Object o) {
      return o;
    }
  }

  static class BadInterfaceImpl2 implements Interface {
    @Override
    public Object returnSource() {
      return InferTaint.inferSecretSource();
    }

    @Override
    public void callSink(Object o) {
      InferTaint.inferSensitiveSink(o);
    }

    @Override
    public Object propagate(Object o) {
      return o;
    }
  }

  static class OkInterfaceImpl implements Interface {
    @Override
    public Object returnSource() {
      return null;
    }

    @Override
    public void callSink(Object o) {
    }

    @Override
    public Object propagate(Object o) {
      return null;
    }
  }

  /** interface tests. for all of these, we should see a warning for both BadInterfaceImpl1 and
   BadInterfaceImpl2, but not OkInterfaceImpl */

  static void returnSourceViaInterfaceBad(Interface i) {
    Object source = i.returnSource();
    InferTaint.inferSensitiveSink(source);
  }

  static void callSinkViaInterfaceBad(Interface i) {
    Object source = InferTaint.inferSecretSource();
    i.callSink(source);
  }

  static void propagateViaInterfaceBad(Interface i) {
    Object source = InferTaint.inferSecretSource();
    Object launderedSource = i.propagate(source);
    InferTaint.inferSensitiveSink(launderedSource);
  }

  static void interfaceOk() {
    Interface i = new OkInterfaceImpl();
    Object source1 = i.returnSource();
    InferTaint.inferSensitiveSink(source1);

    Object source2 = InferTaint.inferSecretSource();
    i.callSink(source2);

    Object launderedSource = i.propagate(source2);
    InferTaint.inferSensitiveSink(launderedSource);
  }

  static class Supertype {
    public Object returnSource() {
      return null;
    }

    public void callSink(Object o) {
    }

    public Object propagate(Object o) {
      return null;
    }
  }

  static class BadSubtype extends Supertype {
    @Override
    public Object returnSource() {
      return InferTaint.inferSecretSource();
    }

    @Override
    public void callSink(Object o) {
      InferTaint.inferSensitiveSink(o);
    }

    @Override
    public Object propagate(Object o) {
      return o;
    }
  }

  static void returnSourceViaSubtypeBad(Supertype s) {
    Object source = s.returnSource();
    InferTaint.inferSensitiveSink(source);
  }

  static void callSinkViaSubtypeBad(Supertype s) {
    Object source = InferTaint.inferSecretSource();
    s.callSink(source);
  }

  static void propagateViaSubtypeBad(Supertype s) {
    Object source = InferTaint.inferSecretSource();
    Object launderedSource = s.propagate(source);
    InferTaint.inferSensitiveSink(launderedSource);
  }

  // need to look and see if we know the concrete type of the receiver to get this one
  static void FP_propagateViaConcreteTypeOk() {
    Supertype s = new Supertype();

    Object source1 = s.returnSource();
    InferTaint.inferSensitiveSink(source1);

    Object source2 = InferTaint.inferSecretSource();
    s.callSink(source2);

    Object launderedSource = s.propagate(source2);
    InferTaint.inferSensitiveSink(launderedSource);
  }

}
