/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.lang;

import java.io.File;
import java.io.IOException;


public class Runtime {

    private Runtime() {
    }

    public Process exec(String command) throws IOException {
        return exec(command, null, null);
    }

    public Process exec(String command, String[] envp) throws IOException {
        return exec(command, envp, null);
    }

    public Process exec(String command, String[] envp, File dir)
            throws IOException {
        return ProcessManager.getInstance().exec(null, envp, null, false);
    }

    public Process exec(String cmdarray[]) throws IOException {
        return exec(cmdarray, null, null);
    }

    public Process exec(String[] cmdarray, String[] envp) throws IOException {
        return exec(cmdarray, envp, null);
    }

    public Process exec(String[] cmdarray, String[] envp, File dir)
            throws IOException {
        return ProcessManager.getInstance().exec(cmdarray, envp, dir, false);
    }

}
