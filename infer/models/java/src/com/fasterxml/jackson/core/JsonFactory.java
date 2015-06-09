/* Jackson JSON-processor.
 *
 * Copyright (c) 2007- Tatu Saloranta, tatu.saloranta@iki.fi
 */
package com.fasterxml.jackson.core;

import com.fasterxml.jackson.core.io.CharacterEscapes;
import com.fasterxml.jackson.core.io.InputDecorator;
import com.fasterxml.jackson.core.io.OutputDecorator;
import com.fasterxml.jackson.core.json.PackageVersion;
import com.fasterxml.jackson.core.json.UTF8StreamJsonParser;
import com.fasterxml.jackson.core.sym.BytesToNameCanonicalizer;
import com.fasterxml.jackson.core.sym.CharsToNameCanonicalizer;
import com.fasterxml.jackson.core.util.BufferRecycler;

import java.io.*;
import java.lang.ref.SoftReference;
import java.net.URL;

public class JsonFactory
        implements Versioned, java.io.Serializable {
    private static long serialVersionUID;

    public static String FORMAT_NAME_JSON;

    protected static int DEFAULT_FACTORY_FEATURE_FLAGS;

    protected static int DEFAULT_PARSER_FEATURE_FLAGS;

    protected static int DEFAULT_GENERATOR_FEATURE_FLAGS;

    private static SerializableString DEFAULT_ROOT_VALUE_SEPARATOR;

    protected static ThreadLocal<SoftReference<BufferRecycler>> _recyclerRef;

    protected transient CharsToNameCanonicalizer _rootCharSymbols;

    protected transient BytesToNameCanonicalizer _rootByteSymbols;

    protected ObjectCodec _objectCodec;

    protected int _factoryFeatures;

    protected int _parserFeatures;

    protected int _generatorFeatures;

    protected CharacterEscapes _characterEscapes;

    protected InputDecorator _inputDecorator;

    protected OutputDecorator _outputDecorator;

    protected SerializableString _rootValueSeparator;

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
