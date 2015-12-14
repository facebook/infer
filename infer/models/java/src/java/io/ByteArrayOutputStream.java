/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.io;

public class ByteArrayOutputStream extends OutputStream implements Closeable {

  public ByteArrayOutputStream() {}

  public ByteArrayOutputStream(int size) {}

  @Override
  public void write(int b) {}

  @Override
  public void write(byte[] b, int off, int len) {}

  public void close() {}

}
