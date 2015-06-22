/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.lang;

import com.facebook.infer.models.InferUndefined;

public class Object {

    public Class getClass() {
        Class c = new Class();
        c.name = InferUndefined.string_undefined();
        return c;
    }

}
