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

import java.io.File;
import java.io.FileDescriptor;
import java.io.IOException;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;



final class ProcessManager {


    public Process exec(String[] taintedCommand, String[] taintedEnvironment, File workingDirectory,
                        boolean redirectErrorStream) throws IOException {

        FileDescriptor in = new FileDescriptor();
        FileDescriptor out = new FileDescriptor();
        FileDescriptor err = new FileDescriptor();

        ProcessImpl process = new ProcessImpl(InferUndefined.int_undefined(), in, out, err);
        return process;
    }

    static class ProcessReference extends WeakReference<ProcessImpl> {

        final int processId;

        public ProcessReference(ProcessImpl referent, ProcessReferenceQueue referenceQueue) {
            super(referent, referenceQueue);
            this.processId = referent.pid;
        }
    }

    static class ProcessReferenceQueue extends ReferenceQueue<ProcessImpl> {
    }

    static class ProcessImpl extends Process {

        ProcessImpl(int pid, FileDescriptor in, FileDescriptor out, FileDescriptor err) {
            super(pid, in, out, err);
        }

        void setExitValue(int exitValue) {
        }
    }

    private static final ProcessManager instance = new ProcessManager();

    public static ProcessManager getInstance() {
        return instance;
    }

}
