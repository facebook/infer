#!/bin/bash -ex

action="$1"
shift

llvm_config=""
libdir=""
cmake=""
make=""
link_mode=""
use_homebrew=FALSE

while [[ $# -gt 0 ]]; do
  case $1 in
  --llvm-config)
    if [[ $# -lt 2 ]]; then
        echo "No llvm-config specified."
        exit 1
    fi
    llvm_config="$2"
    shift 2
    ;;
  --libdir)
    if [[ $# -lt 2 ]]; then
        echo "No libdir specified."
        exit 1
    fi
    libdir="$2"
    shift 2
    ;;
  --cmake)
    if [[ $# -lt 2 ]]; then
        echo "No cmake specified."
        exit 1
    fi
    cmake="$2"
    shift 2
    ;;
  --make)
    if [[ $# -lt 2 ]]; then
        echo "No make specified."
        exit 1
    fi
    make="$2"
    shift 2
    ;;
  --link-mode)
    if [[ $# -lt 2 ]]; then
        echo "No link-mode specified."
        exit 1
    fi
    link_mode="$2"
    shift 2
    ;;
  --use-homebrew)
    use_homebrew=TRUE
    shift
    ;;
  *)
    echo "Unknown option."
    shift
    ;;
  esac
done

function filter_experimental_targets {
    sed 's/ARC//g' | sed 's/CSKY//g' | sed 's/DirectX//g' | sed 's/M68k//g' | sed 's/SPIRV//g' | sed 's/Xtensa//g' | xargs
}

function llvm_build {
    mkdir "build"
    "$cmake" -Bbuild -Sllvm \
        -DCMAKE_BUILD_TYPE="`"$llvm_config" --build-mode`" \
        -DLLVM_TARGETS_TO_BUILD="`"$llvm_config" --targets-built | filter_experimental_targets | sed 's/ /;/g'`" \
        -DLLVM_OCAML_EXTERNAL_LLVM_LIBDIR=`"$llvm_config" --libdir` \
        -DLLVM_LINK_LLVM_DYLIB=`[ $link_mode = "shared" ] && echo TRUE || echo FALSE` \
        -DLLVM_OCAML_OUT_OF_TREE=TRUE \
        -DLLVM_OCAML_INSTALL_PATH="${libdir}" \
        -DLLVM_OCAML_USE_HOMEBREW="${use_homebrew}"
    $make -j -Cbuild ocaml_all
}

function llvm_install {
  if test -d "build"; then
    "$cmake" -Pbuild/bindings/ocaml/cmake_install.cmake
  fi
}

case "$action" in
"build")
  llvm_build
  ;;
"install")
  llvm_install
  ;;
*)
  echo "Action not recognized"
  exit 1
  ;;
esac
