/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package com.fasterxml.jackson.core;

import com.android.internal.util.FileRotator.Reader;
import com.fasterxml.jackson.core.json.PackageVersion;
import com.fasterxml.jackson.core.json.UTF8StreamJsonParser;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
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
