/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;
import java.nio.FileChannelImpl;
import java.nio.channels.FileChannel;

public class FileOutputStream extends OutputStream {

  private FileDescriptor fd;
  private FileChannel channel;

  private void init() {
    InferBuiltins.__set_file_attribute(this);
  }

  public FileOutputStream(String name) throws FileNotFoundException {
    if (InferUndefined.boolean_undefined()) {
      init();
    } else {
      throw new FileNotFoundException();
    }
  }

  public FileOutputStream(String name, boolean append) throws FileNotFoundException {
    if (InferUndefined.boolean_undefined()) {
      init();
    } else {
      throw new FileNotFoundException();
    }
  }

  public FileOutputStream(File file) throws FileNotFoundException {
    if (InferUndefined.boolean_undefined()) {
      init();
    } else {
      throw new FileNotFoundException();
    }
  }

  public FileOutputStream(File file, boolean append) throws FileNotFoundException {
    if (InferUndefined.boolean_undefined()) {
      init();
    } else {
      throw new FileNotFoundException();
    }
  }

  public FileOutputStream(FileDescriptor fdObj) {
    init();
  }

  public FileChannel getChannel() {
    channel = new FileChannelImpl(this, fd, InferUndefined.int_undefined());
    return channel;
  }

  public void write(int b) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(byte b[]) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(byte b[], int off, int len) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void close() throws IOException {
    InferBuiltins.__set_mem_attribute(this);
    InferUndefined.can_throw_ioexception_void();
  }
}
