/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.fasterxml.jackson.core.json;

import com.facebook.infer.builtins.InferUndefined;
import com.fasterxml.jackson.core.Base64Variant;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.core.base.ParserBase;
import com.fasterxml.jackson.core.io.IOContext;
import com.fasterxml.jackson.core.sym.BytesToNameCanonicalizer;
import java.io.IOException;
import java.io.InputStream;

/*
 * Fake UTF8StreamJsonParser
 * This class contains a minimum set of methods in order to compile it for the
 * models
 */
public final class UTF8StreamJsonParser extends ParserBase {

  protected ObjectCodec _objectCodec;

  protected BytesToNameCanonicalizer _symbols;

  protected int[] _quadBuffer;

  protected boolean _tokenIncomplete;

  protected InputStream _inputStream;

  protected byte[] _inputBuffer;

  protected boolean _bufferRecyclable;

  public UTF8StreamJsonParser(
      IOContext ctxt,
      int features,
      InputStream in,
      ObjectCodec codec,
      BytesToNameCanonicalizer sym,
      byte[] inputBuffer,
      int start,
      int end,
      boolean bufferRecyclable) {
    super(ctxt, features);
    _inputStream = in;
    _objectCodec = codec;
    _symbols = sym;
    _inputBuffer = inputBuffer;
    _inputPtr = start;
    _inputEnd = end;
    _bufferRecyclable = bufferRecyclable;
  }

  @Override
  public void close() throws IOException {
    if (_inputStream != null) {
      _inputStream.close();
    }
  }

  private void throwExceptions() throws JsonParseException, IOException {
    if (InferUndefined.boolean_undefined()) {
      throw new JsonParseException(null, null, null);
    }
    if (InferUndefined.boolean_undefined()) {
      throw new IOException();
    }
  }

  /*
   * Methods from ParserBase
   */

  @Override
  protected boolean loadMore() throws IOException {
    return InferUndefined.can_throw_ioexception_boolean();
  }

  @Override
  protected void _finishString() throws IOException, JsonParseException {
    throwExceptions();
  }

  @Override
  protected void _closeInput() throws IOException {
    close();
  }

  /*
   * Methods from ParserMinimalBase
   */

  @Override
  public byte[] getBinaryValue(Base64Variant b64variant) throws IOException, JsonParseException {
    throwExceptions();
    return new byte[] {InferUndefined.byte_undefined()};
  }

  @Override
  public int getTextOffset() throws IOException, JsonParseException {
    throwExceptions();
    return InferUndefined.int_undefined();
  }

  @Override
  public int getTextLength() throws IOException, JsonParseException {
    throwExceptions();
    return InferUndefined.int_undefined();
  }

  @Override
  public char[] getTextCharacters() throws IOException, JsonParseException {
    throwExceptions();
    return new char[] {InferUndefined.char_undefined()};
  }

  @Override
  public String getText() throws IOException, JsonParseException {
    throwExceptions();
    return (String) InferUndefined.object_undefined();
  }

  @Override
  public JsonToken nextToken() throws IOException, JsonParseException {
    throwExceptions();
    throw new IOException();
  }
}
