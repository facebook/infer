/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import static com.facebook.infer.builtins.InferBuiltins.assume;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import java.util.Collection;
import java.util.Map;

public class Validate {

   public static <T> T notNull(T object) {
     assume(object != null);
     return object;
   }

   public static <T> T notNull(T object, String message, Object... values) {
     assume(object != null);
     return object;
   }

   public static <T extends CharSequence> T notEmpty(T chars) {
     assume(chars != null && chars.length() > 0 );
     return chars;
   }

   public static <T extends CharSequence> T notBlank(T chars) {
     assume(chars != null && chars.length() > 0 );
     return chars;
   }

   public static <T extends CharSequence> T notEmpty(T chars, String message, Object... values) {
     return notEmpty(chars);
   }

   public static <T extends CharSequence> T notBlank(T chars, String message, Object... values) {
     return notBlank(chars);
   }

   public static <T extends Collection<?>> T notEmpty(T collection) {
     assume(collection != null && collection.size() > 0 );
     return collection;
   }

   public static <T extends Collection<?>> T notEmpty(T collection, String message, Object... values) {
     return notEmpty(collection);
  }

  public static <T extends Map<?,?>> T notEmpty(T map) {
     assume(map != null);
    return map;
  }

  public static <T extends Map<?,?>> T notEmpty(T map, String message, Object... values) {
     return notEmpty(map);
  }

  public static void isTrue(boolean expression) {
     assume(expression);
  }

  public static void isTrue(boolean expression, String message, double value) {
     assume(expression);
  }

  public static void isTrue(boolean expression, String message, long value) {
     assume(expression);
  }

  public static void isTrue(boolean expression, String message, Object... values) {
     assume(expression);
  }

}
