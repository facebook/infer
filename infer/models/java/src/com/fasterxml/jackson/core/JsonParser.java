/* Jackson JSON-processor.
 *
 * Copyright (c) 2007- Tatu Saloranta, tatu.saloranta@iki.fi
 */

package com.fasterxml.jackson.core;

import com.facebook.infer.models.InferUndefined;
import com.fasterxml.jackson.core.json.UTF8StreamJsonParser;

import java.io.Closeable;
import java.io.IOException;

public abstract class JsonParser
        implements Closeable, Versioned {
    private static int MIN_BYTE_I;
    private static int MAX_BYTE_I;

    private static int MIN_SHORT_I;
    private static int MAX_SHORT_I;

    protected int _features;

    public void close() throws IOException {
        if (this instanceof UTF8StreamJsonParser) {
            ((UTF8StreamJsonParser) this).close();
        }
    }

    private void throwExceptions()
            throws JsonParseException, IOException {
        if (InferUndefined.boolean_undefined()) {
            throw new JsonParseException(null, null, null);
        }
        if (InferUndefined.boolean_undefined()) {
            throw new IOException();
        }
    }

    public Object readValueAs(Class valueType)
            throws IOException, JsonProcessingException {
        throwExceptions();
        return InferUndefined.object_undefined();
    }

}
    
