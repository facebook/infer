/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.io;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;
import com.facebook.infer.builtins.InferUtils;

import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;

public abstract class InputStreamReader {

    public InputStreamReader(InputStream in, String charsetName)
            throws UnsupportedEncodingException {
        if (charsetName == null)
            throw new NullPointerException("charsetName");
        else if (InferUtils.isValidCharset(charsetName)) {
          InferBuiltins.__set_mem_attribute(in);
          InferBuiltins.__set_file_attribute(this);
        } else
            throw new UnsupportedEncodingException();
    }

    public int read() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(char cbuf[]) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(char[] cbuf, int off, int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public boolean ready() throws IOException {
        return InferUndefined.can_throw_ioexception_boolean();
    }


}
