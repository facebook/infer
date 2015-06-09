/*
 * Copyright (C) 2007 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package java.lang;

import com.facebook.infer.models.InferUndefined;

import java.io.*;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.Map;


final class ProcessManager {

    private Map<Integer, ProcessReference> processReferences;

    private ProcessReferenceQueue referenceQueue;

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
