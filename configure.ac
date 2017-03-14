dnl autoconf script for Infer
dnl run ./autogen.sh to generate a configure script
dnl
dnl Copyright (c) 2015 - present Facebook, Inc.
dnl All rights reserved.
dnl
dnl This source code is licensed under the BSD style license found in the
dnl LICENSE file in the root directory of this source tree. An additional grant
dnl of patent rights can be found in the PATENTS file in the same directory.

AC_PREREQ([2.63])

# WARNING: the version number has to be kept in sync with:
# - the values below
# - opam
# - docker/Dockerfile
AC_INIT([Infer],
        [0.10.0],
        [https://github.com/facebook/infer/issues/])

AC_CONFIG_SRCDIR([infer/src/IR/Sil.re])

# WARNING: keep in sync with above
INFER_MAJOR=0
INFER_MINOR=10
INFER_PATCH=0

AC_SUBST([INFER_MAJOR])
AC_SUBST([INFER_MINOR])
AC_SUBST([INFER_PATCH])

# are we in a release source tree
AC_CHECK_FILE([.release], [is_release_tree=yes], [is_release_tree=no])
IS_RELEASE_TREE=$is_release_tree
AC_SUBST([IS_RELEASE_TREE])

# are we in an internal source tree
AC_CHECK_FILE([.facebook], [is_facebook_tree=yes], [is_facebook_tree=no])
IS_FACEBOOK_TREE=$is_facebook_tree
AC_SUBST([IS_FACEBOOK_TREE])

# to compile the facebook-clang-plugins
AC_ARG_VAR([CLANG_PREFIX], [directory where clang is installed (defaults=$PWD/facebook-clang-plugins/clang/install)])
AS_IF([test "x$CLANG_PREFIX" = "x"], [
  CLANG_PREFIX="$(pwd)/facebook-clang-plugins/clang/install"
])

AC_ARG_VAR([CLANG_INCLUDES], [clang headers directories (defaults=$CLANG_PREFIX/include)])
AS_IF([test "x$CLANG_INCLUDES" = "x"], [
  CLANG_INCLUDES="$CLANG_PREFIX/include"
])


AC_ARG_ENABLE(c-analyzers,
  AS_HELP_STRING([--disable-c-analyzers],
    [do not build the C/C++/ObjC analyzers (default is to build them)]),
  ,
  enable_c_analyzers=yes)
BUILD_C_ANALYZERS=$enable_c_analyzers
AC_SUBST([BUILD_C_ANALYZERS])

AC_ARG_ENABLE(java-analyzers,
  AS_HELP_STRING([--disable-java-analyzers],
    [do not build the Java analyzers (default is to build them)]),
  ,
  enable_java_analyzers=yes)

BUILD_JAVA_ANALYZERS=$enable_java_analyzers
AC_SUBST([BUILD_JAVA_ANALYZERS])

AC_ARG_WITH(fcp-clang,
  AS_HELP_STRING([--without-fcp-clang],
    [do not use $CLANG_PREFIX/bin/clang to override the default compiler (default is to override if in an infer release)]),
  ,
  with_fcp_clang=$is_release_tree)

AS_IF([test "x$enable_c_analyzers" = "xyes"], [
  AC_MSG_CHECKING([whether to use the compilers in $CLANG_PREFIX/bin])
  case "$with_fcp_clang" in
    no)
      AC_MSG_RESULT([no])
    ;;
    yes)
      CC=$CLANG_PREFIX/bin/clang
      CXX=$CLANG_PREFIX/bin/clang++
      OBJC=$CLANG_PREFIX/bin/clang
      AC_MSG_RESULT([yes])
    ;;
    *)
      AC_MSG_ERROR([invalid value for --without-fcp-clang; use "yes" or "no"])
    ;;
  esac

  AC_CHECK_TOOL([SHASUM], [shasum], [no])
  AC_ASSERT_PROG([shasum], [$SHASUM])
  # cmake is required to build llvm+clang
  AC_PATH_TOOL([CMAKE], [cmake], [no])
  AS_IF([test "x$is_release_tree" = "xno"], [
    AC_ASSERT_PROG([cmake], [$CMAKE])
  ])
  AC_ARG_ENABLE(ocamlopt-custom-cc,
    AS_HELP_STRING([--enable-ocamlopt-custom-cc], [use CC in ocamlopt invocations]),
    ,
    enable_ocamlopt_custom_cc=no)
  ENABLE_OCAMLOPT_CUSTOM_CC=$enable_ocamlopt_custom_cc
  AC_SUBST([ENABLE_OCAMLOPT_CUSTOM_CC])
])
# end if($enable_c_analyzers)


AC_CHECK_TOOL([PYTHON27], [python2.7], [no])
AC_ASSERT_PROG([python2.7], [$PYTHON27])

AC_PATH_TOOL([XCODE_SELECT], [xcode-select], [no])


# prefer clang over gcc because the plugins makes use of
# clang-specific #pragma's
AC_PROG_CC(clang gcc)
AC_PROG_AWK
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P

if test "x$enable_c_analyzers" = "xyes"; then
  AC_PROG_CPP
  AC_PROG_CXX(clang++ g++)

  dnl clang wants either clang version >= 3.1 or gcc version >= 4.7.2 to
  dnl compile itself
  AC_MSG_CHECKING([if the C/C++ compiler is recent enough])
  AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#ifdef __clang__
#if __clang_major__ < 3 || (__clang_major__ == 3 && __clang_minor__ < 1)
#error compiler is too old
#endif // version check
#elif defined __GNUC__ // __clang__
#if __GNUC__ < 4 || (__GNUC__ == 4 && (__GNUC_MINOR__ < 7 || (__GNUC_MINOR__ == 7 && __GNUC_PATCHLEVEL__ < 2)))
#error compiler is too old
#endif // version check
#endif // __GNUC__
]])],
    [AC_MSG_RESULT([yes])],
    [dnl
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([
Your C/C++ compiler seems to be too old to build clang, which is
required by the facebook-clang-plugins. Please install either
gcc version >= 4.7.2 or clang version >= 3.1.

See the output of `./configure --help` to force the use of a different
C compiler.

Alternatively, you can checkout a release of infer with clang
pre-compiled here:

  https://github.com/facebook/infer/releases/])
   ]
  )

  AC_CHECK_HEADERS([fcntl.h inttypes.h limits.h locale.h malloc.h stddef.h stdint.h stdlib.h string.h sys/mount.h sys/param.h sys/socket.h sys/statfs.h sys/time.h unistd.h wchar.h wctype.h])
fi
# end if($enable_c_analyzers)


# OCaml dependencies
AC_PROG_OCAML
AC_ASSERT_PROG([ocamlc], [$OCAMLC])
# check the version of OCaml
AC_ASSERT_OCAML_MIN_VERSION([4.02.3])
AC_ASSERT_PROG([ocamlopt], [$OCAMLOPT])
AC_ASSERT_PROG([ocamlbuild], [$OCAMLBUILD])
AC_PROG_FINDLIB
AC_PROG_OCAMLLEX
AC_ASSERT_PROG([ocamllex], [$OCAMLLEX])
AC_PATH_TOOL([MENHIR], [menhir], [no])
AC_ASSERT_PROG([menhir], [$MENHIR])
AC_ASSERT_OCAML_PKG([atdgen], [], [1.6.0])
AC_ASSERT_OCAML_PKG([biniou])
AC_ASSERT_OCAML_PKG([camlzip], [zip])
AC_ASSERT_OCAML_PKG([easy-format])
AC_ASSERT_OCAML_PKG([oUnit], [], [2.0.0])
AC_ASSERT_OCAML_PKG([reason])
AC_ASSERT_OCAML_PKG([yojson])
# check for rebuild after checking for Reason since `rebuild` is normally provided by Reason. This
# way if Reason is missing we get a more helpful error message and not "rebuild not found".
AC_PATH_TOOL([REBUILD], [rebuild], [no])
AC_ASSERT_PROG([rebuild], [$REBUILD])

AC_ARG_VAR([CAML_LD_LIBRARY_PATH],
  [Additional directories to search for dynamically-loaded libraries.])

if test "x$enable_java_analyzers" = "xyes"; then
  AC_CHECK_TOOL([JAVA], [java], [no])
  AC_PATH_TOOL([JAVAC], [javac], [no])
  AC_ASSERT_PROG([javac], [$JAVAC])
  AC_ASSERT_PROG([java], [$JAVA])
  AC_ASSERT_OCAML_PKG([javalib], [], [2.3.3])
  AC_ASSERT_OCAML_PKG([sawja], [], [1.5.2])
  AC_ASSERT_OCAML_PKG([ptrees])

  AC_MSG_CHECKING([for JAVA_HOME])
  cat - <<_ACEOF >conftest.java
public class conftest {
  public static void main(String[[]] args) {
    System.out.println(System.getProperty("java.home"));
    System.exit(0);
  }
}
_ACEOF
  rm -f conftest.class
  if $JAVAC conftest.java; then
    rm -f conftest.java
    _USER_JAVA_HOME=$($JAVA -cp . conftest)
    if rm -f conftest.class; then
       USER_JAVA_HOME=$_USER_JAVA_HOME/..
    else
      AC_MSG_ERROR([Could not run test program with $JAVA])
    fi
  else
    rm -f conftest.java
    AC_MSG_ERROR([Could not compile test program with $JAVAC])
  fi
  AC_MSG_RESULT([$USER_JAVA_HOME])
  AC_SUBST([USER_JAVA_HOME])
fi


AC_PATH_TOOL([ATDGEN], [atdgen], [no])
AC_ASSERT_PROG([atdgen], [$ATDGEN])

AC_ARG_ENABLE(ocaml-bin-annot,
  AS_HELP_STRING([--disable-ocaml-bin-annot], [do not build ocaml .cmt files]),
  ,
  enable_ocaml_bin_annot=yes)
ENABLE_OCAML_BINANNOT=$enable_ocaml_bin_annot
AC_SUBST([ENABLE_OCAML_BINANNOT])


# We use Buck to run the Infer tests
AC_MSG_CHECKING([which version of Buck to use])
buckversion_file=.buckversion
oss_buck_version=763cbab643f2d5773fc1a1676301d7de86aef265
fb_buck_version=7c0f2f731244fdb23d58b69520efd0cbad3e8f3a
AS_IF([test x"$is_facebook_tree" = x"yes"],
  [buck_version=$fb_buck_version],
  [buck_version=$oss_buck_version])
echo $buck_version > "$buckversion_file"
AC_MSG_RESULT([$buck_version])


AC_PATH_TOOL([GETCONF], [getconf], [no])
AC_MSG_CHECKING([the number of cpus the build host has])
if test "$GETCONF" != "no"; then
  if test $("$GETCONF" _NPROCESSORS_ONLN); then
    NCPU=$("$GETCONF" _NPROCESSORS_ONLN)
    AC_MSG_RESULT([$NCPU])
  fi
else
  NCPU=1
  AC_MSG_RESULT([failed, defaulting to 1])
fi
AC_SUBST([NCPU])

# optional progs and libraries that, eg build systems to be run in integration tests

AC_PATH_TOOL([ANT], [ant], [no])
if test ! -z "$SANDCASTLE"; then
   BUCK=buck
   AC_SUBST([BUCK])
else
  AC_PATH_TOOL([BUCK], [buck], [no])
fi
AC_ARG_VAR([MVN], [command to execute Maven when running tests])
AS_IF([test "x$MVN" = "x"], [
  AC_PATH_TOOL([MVN], [mvn], [no])
], [
  AC_MSG_RESULT([checking for mvn... $MVN])
])
AC_PATH_TOOL([NDKBUILD], [ndk-build], [no])
if test x"$NDKBUILD" = x"no"; then
# ndk-build not in $PATH, look into potential android NDK install paths and record the absolute path
# to ndk-build
  AC_PATH_PROG([NDKBUILD], [ndk-build], [no],
    [$PATH$PATH_SEPARATOR$ANDROID_NDK$PATH_SEPARATOR/opt/android_ndk/r10e])
fi

AC_CHECK_PYTHON_MODULE([$PYTHON27], [lxml])

AC_PATH_TOOL([XCPRETTY], [xcpretty], [no])

BREW=no
if which brew > /dev/null; then
  BREW="$(which brew)"
fi

# warn about broken pkg-config version for brew users
if test "$BREW" != "no"; then
  if "$BREW" info pkg-config > /dev/null && \
     test x"$(which pkg-config)" = x"/usr/local/bin/pkg-config"; then
    AC_MSG_NOTICE([pkg-config seems to have been installed from Homebrew])
    AC_MSG_CHECKING([for bad version of pkg-config brew package])
    if readlink $(which pkg-config) | grep -q -e '/0.29.1_1/'; then
      AC_MSG_RESULT([found 0.29.1_1])
      AC_MSG_WARN([])
      AC_MSG_WARN([This version of pkg-config is known to cause issues with compiling infer.])
      AC_MSG_WARN([Consider running the following command to get a working version:])
      AC_MSG_WARN([])
      AC_MSG_WARN([  brew remove pkg-config && brew update && brew install pkg-config])
      AC_MSG_WARN([])
    else
      AC_MSG_RESULT([no bad version found])
    fi
  fi
fi

AC_CONFIG_FILES([
  Makefile.autoconf
])

AC_OUTPUT
