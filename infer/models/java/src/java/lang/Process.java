/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.lang;

import com.facebook.infer.builtins.InferUndefined;

import java.io.*;

public abstract class Process {

  protected final int pid;
  protected final InputStream inputStream;
  protected final OutputStream outputStream;
  protected final InputStream errorStream;

  public Process(int pid, FileDescriptor in, FileDescriptor out, FileDescriptor err) {
        this.pid = pid;
        this.inputStream = new ProcessInputStream(in);
        this.outputStream = new ProcessOutputStream(out);
        this.errorStream = new ProcessInputStream(err);
    }
    public int exitValue() {
        return InferUndefined.int_undefined();
    }

    public InputStream getInputStream() {
        return this.inputStream;
    }

    public OutputStream getOutputStream() {
        return this.outputStream;
    }

    public int waitFor() throws InterruptedException {
        return InferUndefined.int_undefined();
    }

    public void destroy() {
        try {
            inputStream.close();
        } catch (IOException e) {}
        try {
            outputStream.close();
        } catch (IOException e) {}
        try {
            errorStream.close();
        } catch (IOException e) {}
    }

    public Process destroyForcibly() {
      destroy();
      return this;
    }

    private static class ProcessInputStream extends FileInputStream {

        private FileDescriptor fd;

        private ProcessInputStream(FileDescriptor fd) {
            super(fd);
            this.fd = fd;
        }

        @Override
        public void close() throws IOException {
            super.close();
        }
    }

    private static class ProcessOutputStream extends FileOutputStream {

        private FileDescriptor fd;

        private ProcessOutputStream(FileDescriptor fd) {
            super(fd);
            this.fd = fd;
        }

        @Override
        public void close() throws IOException {
            super.close();
        }
    }

}
