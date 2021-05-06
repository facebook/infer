/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.infer.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import javax.annotation.Nonnull;
import javax.annotation.meta.TypeQualifierDefault;
import kotlin.annotations.jvm.MigrationStatus;
import kotlin.annotations.jvm.UnderMigration;

@Retention(RetentionPolicy.CLASS)
@Target({ElementType.TYPE})
// These 2 annotations are needed for better interop of @Nullsafe with Kotlin,
// essentially telling it that both params and return values are non-null by
// default.
@Nonnull
@TypeQualifierDefault({ElementType.METHOD, ElementType.PARAMETER})
// This annotation is needed for kotlinc to recognize {@code
// TypeQualifierDefault} without explicitly passing -Xjsr305=strict flag (which
// may be problematic in large codebases).
@UnderMigration(status = MigrationStatus.STRICT)
/**
 * Configures nullability checking mode of annotated classes; a more general version of {@link
 * NullsafeStrict}.
 *
 * <p>To specify the null-checking behaviour we need to define the following.
 *
 * <p>Code can be either *third-party* or *internal*. Whether the code is third-party is defined
 * based on files in third-party signatures repo. This can be set up per-project (see nullsafe
 * documentation for more details on this).
 *
 * <p>Code can be either *checked* or *unchecked*.
 *
 * <ul>
 *   <ol>
 *     An internal class is checked if it's annotated as @Nullsafe or @NullsafeStrict.
 *   </ol>
 *   <ol>
 *     An external method is checked when it's annotated or has nullability signature
 *     defined in third-party signatures repo.
 *   </ol>
 *   <ol>
 *     The code that has built-in models in nullsafe is considered checked (both internal and
 *     third-party).
 *   </ol>
 * </ul>
 *
 * <p>For internal code our default assumption is that lack of {@code @Nullable} annotation is
 * equivalent to presence of {@code @NonNull} annotation.
 *
 * <p>When something is said to be "treated pessimistically" it means that return value is
 * considered to be nullable and parameters to be non-nullable unless they have explicit {@code
 * Nullable} annotations.
 *
 * <p>Now let's consider *usage examples* and describe nullability checking behaviour in each case:
 *
 * <pre>
 * import com.facebook.infer.annotation.Nullsafe;
 *
 * @Nullsafe(Nullsafe.Mode.LOCAL)
 * public class LocallyCheckedAgainstDependencies { ... }
 * // 1. This mode:
 * //    a. Requires that all third-party calls are to checked code. Calls to unchecked third-party
 * //       code are treated pessimistically.
 * //    b. Calls to internal checked code treated as usual based on existing nullability annotations.
 * //    c. Calls to internal unchecked code are allowed and default assumptions on the meaning of
 * //       presence/absence of nullability annotations are in effect.
 * // In other words, we trust all checked and unchecked internal code and
 * // treat it as if all annotations were correct.
 *
 *
 * @Nullsafe(value = Nullsafe.Mode.LOCAL,
 *           trustOnly = @Nullsafe.TrustList({UncheckedComponent.class}))
 * public class LocallyCheckedWithFewTrustedDependencies { ... }
 * // 2. Same as above but calls to unchecked code are allowed only for classes specified
 * //    in {@code trustOnly} list. Calls to other unchecked code are treated pessimistically
 * //    by the checker.
 *
 *
 * @Nullsafe(value = Nullsafe.Mode.LOCAL, trustOnly = @Nullsafe.TrustList({}))
 * public class LocallyCheckedRequiringOnlyCheckedDirectDependencies { ... }
 * // 3. Same as above but with empty trust list, which means that all calls to unchecked code
 * //    are treated pessimistically.
 * // In practice, it usually means that all direct dependencies of the class
 * // should be @Nullsafe themselves.
 *
 *
 * @Nullsafe(Nullsafe.Mode.STRICT)
 * public class StrictClass { ... }
 * // 4. This is analogous to marking a class as @NullsafeStrict. In particular:
 * //    a. Third-party calls should be to checked code. Calls to unchecked third-party
 * //       code are treated pessimistically.
 * //    b. Only calls to internal classes checked under strict mode are trusted.
 * //    c. All other calls to checked and unchecked code are treated pessimistically.
 * // In practice it means that transitive dependencies of the class should
 * // themselves be @NullsafeStrict.
 * </pre>
 */
public @interface Nullsafe {
  enum Mode {
    LOCAL,
    /**
    * @deprecated STRICT mode is deprecated and soon will have no effect on the
    * behaviour of NullsafeX.
    */
    @Deprecated
    STRICT
  }

  /**
   * @deprecated Explicit TrustList is deprecated and soon will have no effect on the
   * behaviour of Nullsafe. The code will be checked as if the mode is {@code
   * trustAll = true}.
   */
  @Deprecated
  @interface TrustList {

    Class[] value();

    /**
     * Analogous to a wildcard "*" in a trust list when set to true. When set to false only classes
     * listed in {@code value} parameter are trusted.
     */
    boolean trustAll() default false;
  }

  /**
   * Specifies the null-checking mode. The parameter is named {@code value} instead of {@code mode}
   * to enable a single-element annotation shorthand for @Nullsafe annotation, i.e.
   * use @Nullsafe(Nullsafe.Mode.Strict) instead of @Nullsafe(mode = Nullsafe.Mode.STRICT).
   */
  Mode value();

  /**
   * Provides fine-grained control over which unchecked internal classes to trust. Only affects
   * LOCAL null-checking mode, as strict requires all dependencies to be STRICT themselves.
   *
   * @deprecated See {@link TrustList}.
   */
  @Deprecated
  TrustList trustOnly() default
      @TrustList(
          value = {},
          trustAll = true);
}
