dnl Copyright (c) Facebook, Inc. and its affiliates.
dnl
dnl This source code is licensed under the MIT license found in the
dnl LICENSE file in the root directory of this source tree.

dnl autoconf script for Infer
dnl run ./autogen.sh to generate a configure script

AC_PREREQ([2.63])

# WARNING: the version number has to be kept in sync with:
# - the values below
# - opam
AC_INIT([Infer],
        [1.2.0],
        [https://github.com/facebook/infer/issues/])

AC_CONFIG_SRCDIR([infer/src/istd/IStd.ml])

# WARNING: keep in sync with above
INFER_MAJOR=1
INFER_MINOR=2
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

# are we in git repo
AC_CHECK_FILE([.git], [is_git_repo=yes], [is_git_repo=no])
IS_GIT_REPO=$is_git_repo
AC_SUBST([IS_GIT_REPO])

AC_ARG_VAR([PATH], [the shell's $PATH list of directories to search for executables])

# to compile the facebook-clang-plugins
AC_ARG_VAR([CLANG_PREFIX], [directory where clang is installed (defaults=$PWD/facebook-clang-plugins/clang/install)])
AS_IF([test "x$CLANG_PREFIX" = "x"], [
  CLANG_PREFIX="$(pwd)/facebook-clang-plugins/clang/install"
])

AC_ARG_VAR([CLANG_INCLUDES], [clang headers directories (defaults=$CLANG_PREFIX/include)])
AS_IF([test "x$CLANG_INCLUDES" = "x"], [
  CLANG_INCLUDES="$CLANG_PREFIX/include"
])

BUILD_PLATFORM=unknown
WINDOWS_BUILD=no
AC_MSG_CHECKING([for build platform])
# see https://stackoverflow.com/questions/714100/os-detecting-makefile
# but we do this in the configure for homogeneity
case "${OS}" in
    Windows_NT*)
        BUILD_PLATFORM=Windows
        ;;
    *)
        uname_str=`uname -s`
        case "${uname_str}" in
            Linux*)
                BUILD_PLATFORM=Linux
                ;;
            Darwin*)
                BUILD_PLATFORM=Darwin
                ;;
            cygwin*|mingw*)
                BUILD_PLATFORM=Windows
                ;;
            *)
                AC_MSG_ERROR(["OS $uname_str is not supported"])
                ;;
        esac
esac
AC_MSG_RESULT([$BUILD_PLATFORM])
AC_SUBST([BUILD_PLATFORM])

AC_MSG_CHECKING([for build cpu])
[BUILD_CPU=`uname -m`]
AC_MSG_RESULT([$BUILD_CPU])
AC_SUBST([BUILD_CPU])

AC_MSG_CHECKING([for Windows build])
AS_IF([test x"$BUILD_PLATFORM" = x"Windows"], [WINDOWS_BUILD=yes])
AC_MSG_RESULT([$WINDOWS_BUILD])
AC_SUBST([WINDOWS_BUILD])

AC_MSG_CHECKING([for realpath])
AS_IF([test x"$BUILD_PLATFORM" = x"Darwin"], [REALPATH=grealpath], [REALPATH=realpath])
AC_MSG_RESULT([$REALPATH])
AC_SUBST([REALPATH])

AC_ARG_VAR([PLATFORM_ENV], [Build and link binaries against this platform rather than system default (Linux only)])

AC_ARG_ENABLE(c-analyzers,
  AS_HELP_STRING([--disable-c-analyzers],
    [do not build the C/C++/ObjC analyzers (default is to build them)]),
  ,
  enable_c_analyzers=yes)
BUILD_C_ANALYZERS=$enable_c_analyzers
AC_SUBST([BUILD_C_ANALYZERS])

AC_ARG_ENABLE(erlang-analyzers,
  AS_HELP_STRING([--disable-erlang-analyzers],
    [do not build the Erlang analyzers (default is to build them)]),
  ,
  enable_erlang_analyzers=yes)
BUILD_ERLANG_ANALYZERS=$enable_erlang_analyzers
AC_SUBST([BUILD_ERLANG_ANALYZERS])

AC_ARG_ENABLE(hack-analyzers,
  AS_HELP_STRING([--disable-hack-analyzers],
    [do not build the Hack analyzers (default is to build them)]),
  ,
  enable_hack_analyzers=yes)
BUILD_HACK_ANALYZERS=$enable_hack_analyzers
AC_SUBST([BUILD_HACK_ANALYZERS])

AC_ARG_ENABLE(java-analyzers,
  AS_HELP_STRING([--disable-java-analyzers],
    [do not build the Java analyzers (default is to build them)]),
  ,
  enable_java_analyzers=yes)
BUILD_JAVA_ANALYZERS=$enable_java_analyzers
AC_SUBST([BUILD_JAVA_ANALYZERS])

enable_python_analyzers_default=auto
AS_IF([test "x$is_release_tree" = "xyes"], [
  enable_python_analyzers_default=yes
])

AC_ARG_ENABLE(python-analyzers,
  AS_HELP_STRING([--disable-python-analyzers],
    [do not build the Python analyzers (default is to build them in release mode, otherwise auto-detect)]),
  ,
  enable_python_analyzers=$enable_python_analyzers_default)
BUILD_PYTHON_ANALYZERS=$enable_python_analyzers
AC_SUBST([BUILD_PYTHON_ANALYZERS])

AC_ARG_WITH(fcp-clang,
  AS_HELP_STRING([--with-fcp-clang],
    [use $CLANG_PREFIX/bin/clang to override the default compiler (default is not to override)]),
  ,
  with_fcp_clang=no)

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
  AC_CHECK_TOOL([CMAKE], [cmake], [no])
  AC_ASSERT_PROG([cmake], [$CMAKE])
  AC_ARG_ENABLE(ocamlopt-custom-cc,
    AS_HELP_STRING([--enable-ocamlopt-custom-cc], [use CC in ocamlopt invocations]),
    ,
    enable_ocamlopt_custom_cc=no)
  ENABLE_OCAMLOPT_CUSTOM_CC=$enable_ocamlopt_custom_cc
  AC_SUBST([ENABLE_OCAMLOPT_CUSTOM_CC])
])
# end if($enable_c_analyzers)

AC_CHECK_TOOL([PYTHON], [python3], [no])
AS_IF([test "x$enable_python_analyzers" != "xno"], [
  AC_ASSERT_PROG([python3], [$PYTHON])
  AC_MSG_CHECKING([python version])
  python_version=$("$PYTHON" --version)
  AC_MSG_RESULT([$python_version])
  case $python_version in
    Python\ 3.8.*) BUILD_PYTHON_ANALYZERS=yes; break;;
    *) BUILD_PYTHON_ANALYZERS=no;
      AS_IF([test "x$enable_python_analyzers" = "xauto"], [
        AC_MSG_WARN([python version $python_version is not supported, disabling python support])
        ], [
        AC_MSG_ERROR([python version $python_version is not supported, please install python version 3.8 instead or disable python support with --disable-python-analyzers])
      ]); break;;
  esac
]) # end if($enable_python_analyzers)

AC_ARG_VAR([PYTHONHOME], [the location of the standard Python libraries])
AC_SUBST([PYTHONHOME])

AC_CHECK_TOOL([XCODE_SELECT], [xcode-select], [no])
AS_IF([test "x$XCODE_SELECT" != "xno"], [XCODE_SELECT_OUT=`xcode-select -p`])

AC_ARG_VAR([XCODE_BASE], [Install location of xcode])
AS_IF(
  [test "x$XCODE_BASE" != "x"],
        [AC_CHECK_FILE($XCODE_BASE,[HAS_OBJC=yes],[HAS_OBJC=no])],
  [test "x$XCODE_SELECT" != "xno"],
        [XCODE_BASE=$XCODE_SELECT_OUT HAS_OBJC=yes],
  [HAS_OBJC=no])

AC_CHECK_TOOL([XCRUN], [xcrun], [no])

AC_ARG_VAR([SDKROOT], [path to the OSX platform SDK used by clang])
AS_IF(
  [test "x$SDKROOT" = "x" && test "x$XCRUN" != "xno"],
        [SDKROOT=`xcrun --sdk macosx --show-sdk-path`]
)

AC_SUBST([XCODE_BASE])
AC_SUBST([HAS_OBJC])
AC_SUBST([SDKROOT])


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
  # AC_PROG_CXX doesn't set CXX to "no" in case of failure (I have no words...); from the manual:
  # "If none of those checks succeed, then as a last resort set CXX to g++. "
  AS_IF([$CXX --version > /dev/null], [], [AC_MSG_ERROR([no C++ compiler found])])

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

Alternatively, you can checkout a binary release of infer:

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
AC_ASSERT_OCAML_MIN_VERSION([4.06.1])
AC_ASSERT_PROG([ocamlopt], [$OCAMLOPT])
AC_CHECK_TOOL([OCAMLBUILD], [ocamlbuild], [no])
AC_ASSERT_PROG([ocamlbuild], [$OCAMLBUILD])
AC_PROG_FINDLIB
AC_PROG_OCAMLLEX
AC_ASSERT_PROG([ocamllex], [$OCAMLLEX])
AC_CHECK_TOOL([MENHIR], [menhir], [no])
AC_ASSERT_PROG([menhir], [$MENHIR])
AC_ASSERT_OCAML_PKG([atdgen])
AC_ASSERT_OCAML_PKG([biniou])
AC_ASSERT_OCAML_PKG([camlzip], [zip])
AC_ASSERT_OCAML_PKG([easy-format])
AC_ASSERT_OCAML_PKG([oUnit])
AC_CHECK_TOOL([UTOP], [utop], [no])
AC_ASSERT_OCAML_PKG([yojson])

AC_CHECK_TOOL([OCAMLFORMAT], [ocamlformat], [no])

AC_ARG_VAR([CPATH], [Additional directories to search for C headers.])
AC_ARG_VAR([LIBRARY_PATH], [Additional directories to search for C shared objects.])
AC_ARG_VAR([CAML_LD_LIBRARY_PATH],
  [Additional directories to search for dynamically-loaded libraries.])
AC_ARG_VAR([OPAMROOT], [Root of the local opam installation.])
AC_ARG_VAR([OPAMSWITCH], [Opam switch used for building infer.])

AC_CHECK_TOOL([OPAM], [opam], [no])
AS_IF([test "$OPAM" != "no"], [
  AC_MSG_CHECKING([opam version])
  opam_version=$("$OPAM" --version)
  case $opam_version in
    2.*) AC_MSG_RESULT([$opam_version]); break;;
    *) AC_MSG_ERROR([opam version $opam_version is not supported, please install opam version 2 instead]); break;;
  esac
  AC_MSG_CHECKING([current opam root])
  OPAMROOT=$("$OPAM" var root)
  AC_MSG_RESULT([$OPAMROOT])
  AC_MSG_CHECKING([current opam switch])
  OPAMSWITCH=$("$OPAM" switch show)
  AC_MSG_RESULT([$OPAMSWITCH])
], [
  OPAMROOT=no
  OPAMSWITCH=no
])
AC_SUBST([OPAMROOT])
AC_SUBST([OPAMSWITCH])

if test "x$enable_erlang_analyzers" = "xyes"; then
  AC_CHECK_TOOL([ESCRIPT], [escript], [no])
  AC_CHECK_TOOL([ERLC], [erlc], [no])
  AC_CHECK_TOOL([REBAR3], [rebar3], [no])
fi
# end if($enable_erlang_analyzers)

AC_PATH_TOOL([HACKC], [hackc], [no])
AC_MSG_CHECKING([if hackc can run])
AS_IF([$HACKC --version >/dev/null 2>/dev/null],
  [AC_MSG_RESULT([yes])],
  [HACKC=no
   AC_MSG_RESULT([no])])

if test "x$enable_java_analyzers" = "xyes"; then
  AC_CHECK_TOOL([JAVA], [java], [no])
  AC_CHECK_TOOL([JAVAC], [javac], [no])
  AC_ASSERT_PROG([javac], [$JAVAC])
  AC_ASSERT_PROG([java], [$JAVA])
  AC_ASSERT_OCAML_PKG([javalib])
  AC_ASSERT_OCAML_PKG([sawja])

  AC_ARG_VAR([KOTLIN_HOME], [Root of a Kotlin installation.])
  AC_CHECK_TOOL([KOTLINC], [kotlinc], [no])

  JAVAC_VERSION=`"$JAVAC" -version 2>&1`
  AC_MSG_NOTICE([output of javac --version: $JAVAC_VERSION])
  AC_MSG_CHECKING([for Java major version])
  JAVA_MAJOR_VERSION=`"$JAVAC" -version 2>&1 | head -n 1 | cut -d ' ' -f 2`
  JAVA_MAJOR_VERSION_FIRST_NUMBER=`echo $JAVA_MAJOR_VERSION | cut -d '.' -f 1`
  AS_IF([test "x$JAVA_MAJOR_VERSION_FIRST_NUMBER" = "x1"], [
    # version 1.8.xx -> 8
    JAVA_MAJOR_VERSION=`echo $JAVA_MAJOR_VERSION | cut -d . -f 2`
  ], [
    # otherwise pick the first number as the major version
    JAVA_MAJOR_VERSION=`echo $JAVA_MAJOR_VERSION | tr - . | cut -d . -f 1`
  ])
  AC_MSG_RESULT([$JAVA_MAJOR_VERSION])
  AC_SUBST([JAVA_MAJOR_VERSION])

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
  if "$JAVAC" conftest.java; then
    rm -f conftest.java
    _USER_JAVA_HOME=$($JAVA -cp . conftest)
    if rm -f conftest.class; then
       [javac_version_10_or_more=`echo "$JAVA_MAJOR_VERSION" | grep -q -e '^1[0-9]' && echo yes`]
       if test "x$javac_version_10_or_more" = "xyes"; then
          USER_JAVA_HOME=$_USER_JAVA_HOME
       else
          USER_JAVA_HOME=$_USER_JAVA_HOME/..
       fi
    else
      AC_MSG_ERROR([Could not run test program with $JAVA])
    fi
  else
    rm -f conftest.java
    AC_MSG_ERROR([Could not compile test program with $JAVAC])
  fi
  AC_MSG_RESULT([$USER_JAVA_HOME])
  AC_SUBST([USER_JAVA_HOME])

  AC_CHECK_LIB([z], [inflateEnd], [ZLIB_FOUND=yes], [ZLIB_FOUND=no])
  AS_IF([test x"$ZLIB_FOUND" = xno], [AC_MSG_ERROR([zlib not found.])])
fi
# end if($enable_java_analyzers)

AC_CHECK_TOOL([ATDGEN], [atdgen], [no])
AC_ASSERT_PROG([atdgen], [$ATDGEN])

AC_ARG_ENABLE(ocaml-bin-annot,
  AS_HELP_STRING([--disable-ocaml-bin-annot], [do not build ocaml .cmt files]),
  ,
  enable_ocaml_bin_annot=yes)
ENABLE_OCAML_BINANNOT=$enable_ocaml_bin_annot
AC_SUBST([ENABLE_OCAML_BINANNOT])


# We use Buck to run the Infer tests
AS_IF([test x"$is_facebook_tree" = x"yes"],
  AC_MSG_CHECKING([which .buckjavaversion to use])
  cp "facebook/dependencies/dotbuckjavaversion" ".buckjavaversion"
  AC_MSG_RESULT([facebook/dependencies/dotbuckjavaversion])

  AC_MSG_CHECKING([which .buck-java11 to use])
  cp "facebook/dependencies/dotbuck-java11" ".buck-java11"
  AC_MSG_RESULT([facebook/dependencies/dotbuck-java11]))


AC_CHECK_TOOL([GETCONF], [getconf], [no])
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

AC_CHECK_TOOL([ANT], [ant], [no])
AC_CHECK_TOOL([BUCK], [buck1], [no])
AC_CHECK_TOOL([EMACS], [emacs], [no])
AC_ARG_VAR([MVN], [command to execute Maven when running tests])
AS_IF([test "x$MVN" = "x"], [
  AC_CHECK_TOOL([MVN], [mvn], [no])
], [
  AC_MSG_RESULT([checking for mvn... $MVN])
])
AC_CHECK_TOOL([NDKBUILD], [ndk-build], [no])
if test x"$NDKBUILD" = x"no"; then
# ndk-build not in $PATH, look into potential android NDK install paths and record the absolute path
# to ndk-build
  AC_PATH_PROG([NDKBUILD], [ndk-build], [no],
    [$PATH$PATH_SEPARATOR$ANDROID_NDK$PATH_SEPARATOR/opt/android_ndk/r20])
fi

AC_CHECK_TOOL([NINJA], [ninja], [no])
AC_CHECK_TOOL([XCPRETTY], [xcpretty], [no])

AC_CHECK_TOOL([SED], [sed], [no])
AS_IF([test "$SED" != "xno"], [
  AC_MSG_CHECKING([if sed is GNU sed])
  AS_IF(["$SED" --version 2> /dev/null | grep -q -e "GNU sed"], [
    GNU_SED="$SED"
    AC_MSG_RESULT([yes])
  ], [
    AC_MSG_RESULT([no])
    AC_CHECK_TOOL([GNU_SED], [gsed], [no])
  ])
])
AC_SUBST([GNU_SED])


AC_CHECK_TOOL([BREW], [brew], [no])
AC_CHECK_TOOL([INSTALL_NAME_TOOL], [install_name_tool], [no])
AC_SUBST([INSTALL_NAME_TOOL])
AC_CHECK_TOOL([LDD], [ldd], [no])
AC_SUBST([LDD])
AC_CHECK_TOOL([OTOOL], [otool], [no])
AC_SUBST([OTOOL])
AC_CHECK_TOOL([PATCHELF], [patchelf], [no])
AC_SUBST([PATCHELF])

AC_CHECK_TOOL([HH_SINGLE_TYPE_CHECK], [hh_single_type_check], [no])

AC_CHECK_INFER_MAN_LAST_MODIFIED()

AC_CONFIG_FILES([
  Makefile.autoconf
])

AC_OUTPUT

enabled_languages=""
enabled_sequence_sep=""
disabled_languages=""
disabled_sequence_sep=""
AC_DEFUN([AC_CLASSIFY_FRONTEND_SUPPORT],
[dnl
  AS_IF([test "$2" = "yes"],
  [dnl
    enabled_languages=${enabled_languages}${enabled_sequence_sep}$1;
    enabled_sequence_sep=", "
  ], [dnl
    disabled_languages=${disabled_languages}${disabled_sequence_sep}$1;
    disabled_sequence_sep=", "
  ])
])

AC_CLASSIFY_FRONTEND_SUPPORT([clang], [$BUILD_C_ANALYZERS])
AC_CLASSIFY_FRONTEND_SUPPORT([erlang], [$BUILD_ERLANG_ANALYZERS])
AC_CLASSIFY_FRONTEND_SUPPORT([hack], [$BUILD_HACK_ANALYZERS])
AC_CLASSIFY_FRONTEND_SUPPORT([java], [$BUILD_JAVA_ANALYZERS])
AC_CLASSIFY_FRONTEND_SUPPORT([python], [$BUILD_PYTHON_ANALYZERS])

AC_MSG_NOTICE([enabled language frontends:  $enabled_languages])
AC_MSG_NOTICE([disabled language frontends: $disabled_languages])
