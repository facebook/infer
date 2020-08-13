/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe;

import com.facebook.infer.annotation.Nullsafe;
import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import java.util.Random;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;
import javax.annotation.Nullable;

public class Lambdas {
  // Helper
  @Nullable private String nullableStringField;
  private String notNullStringField = "NotNull";

  // Helper
  static interface NullableFunction<T, R> {
    @Nullable
    public R apply(@Nullable T t);
  }

  // Helper
  @Nullable
  private static <T, R> R nullableApply(@Nullable T t, NullableFunction<T, R> f) {
    return f.apply(t);
  }

  // Helper
  static interface NullableCallbackWithDefaultMethods {
    default void onSuccess() {
      // no-op
    }

    void onFailure(@Nullable Integer statusCode);
  }

  // Helper
  private static void acceptNullableCallback(NullableCallbackWithDefaultMethods cb) {
    cb.onFailure(null);
  }

  // Helper
  private static String acceptNullableStringSuffix(String s, @Nullable String suffix) {
    return suffix == null ? s : suffix;
  }

  // Helper
  private static String acceptNotNullString(String s) {
    return s;
  }

  // Helper
  @Nullable
  private static String getNullableString() {
    return new Random().nextBoolean() ? "NotNull" : null;
  }

  private void useLambdaAllNotNull_OK(Stream<String> stream) {
    stream.map(s -> acceptNullableStringSuffix(s, "IAmNotNull"));
  }

  private void useLambdaNullableFieldCapture_OK(Stream<String> stream) {
    stream.map(s -> acceptNullableStringSuffix(s, nullableStringField));
  }

  private void useLambdaNullableLocalCapture_OK(Stream<String> stream) {
    String localString = getNullableString();
    stream.map(s -> acceptNullableStringSuffix(s, localString));
  }

  private void useLambdaNullableLocalCaptureToNotNull_BAD_FN(Stream<String> stream) {
    String localString = getNullableString(); // @Nullable
    stream.map(s -> acceptNotNullString(localString)); // BAM! needs NotNull
  }

  private String useLambdaToSetFieldToNull_BAD_FN() {
    Consumer<String> f =
        s -> {
          if (s.length() < 42) {
            notNullStringField = null; // BAM!
          }
        };

    f.accept("TheString");

    return notNullStringField;
  }

  @Nullable
  private Integer useLambdaWithNullableInterface_BAD_WRONG(String paramString) {
    // should be "nullable dereference" here, while now it's "inconsistent subclass param"
    return nullableApply(paramString, lambdaString -> lambdaString.length());
  }

  @Nullable
  private Integer useLambdaWithExplicitParamTypes_BAD(String paramString) {
    // expected "inconsistent subclass annotation" here, since parent defined param as @Nullable
    return nullableApply(paramString, (String lambdaString) -> lambdaString.length());
  }

  @Nullable
  private Integer useLambdaWithExplicitParamTypesAndNullability_BAD_WRONG(String paramString) {
    // should flag "nullable derefernce"; instead raises "inconsistent subclass"
    // because it doesn't see the @Nullable annotation on the lambda's param
    return nullableApply(paramString, (@Nullable String lambdaString) -> lambdaString.length());
  }

  private void useLambdaForInterfaceWithDefaultMethods_OK_FP() {
    acceptNullableCallback(statusCode -> System.out.println(statusCode)); // should be OK
  }

  // Following tests demonstrates some "desired" behaviours of typechecker in
  // "lambda pipelines". However, those will require significant investment to
  // implement.

  // Currently, it reports nothing because we lack a model for java.util.function.Function
  private void useLambdasInStreamPipeline_WRONG() {
    Stream.generate(() -> getNullableString())
        .peek(s -> System.out.println(s.length())) // s is @Nullable => nullable dereference!
        .flatMap(
            s -> s == null ? Stream.empty() : Stream.of(s)) // should be a stream of notnull strings
        .forEach(s -> System.out.println(s.length())); // fine
  }

  // Helper
  // Starting with Java 8 annotations can be applied to type parameters.
  // Both JSR-305's @Nullable and Android's @Nullable however lack necessary
  // @Target qualifier, so we define our own here.
  @Target({ElementType.TYPE_PARAMETER, ElementType.TYPE_USE})
  public @interface MyNullable {};

  @Nullsafe(Nullsafe.Mode.LOCAL)
  static class NullsafeClass {
    // Cannot reasonably use without some tricky internal model
    private String useJavaUtilFunction_UNSUPPORTED(Function<@MyNullable String, String> f) {
      return f.apply(getNullableString());
    }
  }
}
