/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.fasterxml.jackson.core;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;
import java.io.Closeable;
import java.io.IOException;

public abstract class JsonParser implements Closeable, Versioned {

  public void close() throws IOException {
    InferBuiltins.__set_mem_attribute(this);
    InferUndefined.can_throw_ioexception_void();
  }

  private void throwExceptions() throws JsonParseException, IOException {
    if (InferUndefined.boolean_undefined()) {
      throw new JsonParseException(null, null, null);
    }
    if (InferUndefined.boolean_undefined()) {
      throw new IOException();
    }
  }

  public Object readValueAs(Class valueType) throws IOException, JsonProcessingException {
    throwExceptions();
    return InferUndefined.object_undefined();
  }
}
