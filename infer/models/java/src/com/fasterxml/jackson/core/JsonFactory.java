/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.fasterxml.jackson.core;

import com.fasterxml.jackson.core.json.PackageVersion;
import com.fasterxml.jackson.core.json.UTF8StreamJsonParser;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.net.URL;

public class JsonFactory
        implements Versioned, java.io.Serializable {

    @Override
    public Version version() {
        return PackageVersion.VERSION;
    }

    public JsonParser createParser(File f)
            throws IOException, JsonParseException {
        return createOwningParser();
    }

    public JsonParser createParser(URL url)
            throws IOException, JsonParseException {
        return createOwningParser();
    }

    public JsonParser createParser(InputStream in)
            throws IOException, JsonParseException {
        return createNonOwningParser();
    }

    public JsonParser createParser(Reader r)
            throws IOException, JsonParseException {
        return createNonOwningParser();
    }

    private JsonParser createOwningParser()
            throws IOException, JsonParseException {
        InputStream in = new FileInputStream("");
        return new UTF8StreamJsonParser(null, 0, in, null, null,
                new byte[]{}, 0, 0,
                false);
    }

    private JsonParser createNonOwningParser()
            throws IOException, JsonParseException {
        return new UTF8StreamJsonParser(null, 0, null, null, null,
                new byte[]{}, 0, 0,
                false);
    }

}
