/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

 package codetoanalyze.java.pulse;

 import java.io.File;

public class Messages {
    public void customSourceAndSinkBad() {
        inferSensitiveSink(inferSecretSource());
    }

    public void userControlledInputToVulnerableSinkBad() {
        vulnerableSink(userControlledSource());
    }

    public void sanitizedUserControlledInputOk() {
        vulnerableSink(sanitizeInput(userControlledSource()));
    }

    String inferSecretSource() {
        return "";
    }

    void inferSensitiveSink(String any) {
    }

    String userControlledSource() {
        return "";
    }

    void vulnerableSink(String any) {
    }

    String sanitizeInput(String any) {
        return any;
    }
}
