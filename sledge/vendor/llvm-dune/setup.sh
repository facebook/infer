#!/bin/sh -e

if test "$(dirname $0)" != '.'; then
    echo "The script must be executed from its current directory."
    exit 1
fi

if test "$#" -ne 1; then
    echo "Usage: $0 <llvm-config>"
    exit 1
fi

llvm_config=$1
default_mode=
support_static_mode=false
support_shared_mode=false

llvm_config() {
    "$llvm_config" $@
}

if llvm_config --link-static; then
    default_mode=static
    support_static_mode=true
fi

if llvm_config --link-shared; then
    default_mode=shared
    support_shared_mode=true
fi

if test -z "$default_mode"; then
    echo "Something is wrong with the llvm-config command provided."
    exit 1
fi

base_cflags=$(llvm_config --cflags)
ldflags="$(llvm_config --ldflags) -lstdc++"
llvm_targets=$(llvm_config --targets-built)

rm -rf src
cp -r llvm-project/llvm/bindings/ocaml src

create_dune_file() {
    findlibname=$1
    dirname=$2
    modname=$3
    cfile=$4
    depends=$5
    components=$6

    if test "$dirname" = "backends"; then
        basedir=src/$dirname/$components
        test ! -d "$basedir" && mkdir "$basedir"
        sed "s/@TARGET@/$components/g" "src/$dirname/llvm_backend.ml.in" > "$basedir/$modname.ml"
        sed "s/@TARGET@/$components/g" "src/$dirname/llvm_backend.mli.in" > "$basedir/$modname.mli"
        sed "s/@TARGET@/$components/g" "src/$dirname/backend_ocaml.c" > "$basedir/$cfile.c"
        cflags="$base_cflags \"-DTARGET=$components\""
    else
        basedir=src/$dirname
        cflags=$base_cflags
    fi

    test ! -d "$basedir/common" && mkdir "$basedir/common"
    cp "$basedir/$modname.mli" "$basedir/common"

    echo "
(library
 (name $modname)
 (public_name $findlibname)
 (wrapped false)
 (virtual_modules $modname)
 (libraries $depends)
 (default_implementation $findlibname.$default_mode))
" > "$basedir/common/dune"

    if $support_shared_mode; then
        test ! -d "$basedir/shared" && mkdir "$basedir/shared"
        cp "$basedir/$modname.ml" "$basedir/shared"
        cp "$basedir/$cfile.c" "$basedir/shared"

        echo "
(library
 (name ${modname}_shared)
 (public_name $findlibname.shared)
 (implements $findlibname)
 (foreign_stubs
  (language c)
  (names ${cfile})
  (flags ($cflags)))
 (c_library_flags ($ldflags $(llvm_config --system-libs --link-shared --libs $components))))
" >> "$basedir/shared/dune"
    fi

    if $support_static_mode; then
        test ! -d "$basedir/static" && mkdir "$basedir/static"
        cp "$basedir/$modname.ml" "$basedir/static"
        cp "$basedir/$cfile.c" "$basedir/static"

        echo "
(library
 (name ${modname}_static)
 (public_name $findlibname.static)
 (implements $findlibname)
 (foreign_stubs
  (language c)
  (names ${cfile})
  (flags ($cflags)))
 (c_library_flags ($ldflags $(llvm_config --system-libs --link-static --libs $components))))
" >> "$basedir/static/dune"
    fi

    rm "$basedir/$modname.ml"
    rm "$basedir/$cfile.c"
}

# ------------------ public name -------- directory ---------------- module name -------- C file name --------- OCaml dependencies -------------- LLVM components (for the linker)
create_dune_file     llvm                 llvm                       llvm                 llvm_ocaml            ""                                "core support"
create_dune_file     llvm.analysis        analysis                   llvm_analysis        analysis_ocaml        "llvm"                            "analysis"
create_dune_file     llvm.bitreader       bitreader                  llvm_bitreader       bitreader_ocaml       "llvm"                            "bitreader"
create_dune_file     llvm.bitwriter       bitwriter                  llvm_bitwriter       bitwriter_ocaml       "llvm unix"                       "bitwriter"
create_dune_file     llvm.executionengine executionengine            llvm_executionengine executionengine_ocaml "llvm llvm.target ctypes.foreign" "executionengine mcjit native"
create_dune_file     llvm.ipo             transforms/ipo             llvm_ipo             ipo_ocaml             "llvm"                            "ipo"
create_dune_file     llvm.irreader        irreader                   llvm_irreader        irreader_ocaml        "llvm"                            "irreader"
create_dune_file     llvm.scalar_opts     transforms/scalar_opts     llvm_scalar_opts     scalar_opts_ocaml     "llvm"                            "scalaropts"
create_dune_file     llvm.transform_utils transforms/utils           llvm_transform_utils transform_utils_ocaml "llvm"                            "transformutils"
create_dune_file     llvm.vectorize       transforms/vectorize       llvm_vectorize       vectorize_ocaml       "llvm"                            "vectorize"
create_dune_file     llvm.passmgr_builder transforms/passmgr_builder llvm_passmgr_builder passmgr_builder_ocaml "llvm"                            "ipo"
create_dune_file     llvm.target          target                     llvm_target          target_ocaml          "llvm"                            "target"
create_dune_file     llvm.linker          linker                     llvm_linker          linker_ocaml          "llvm"                            "linker"
create_dune_file     llvm.all_backends    all_backends               llvm_all_backends    all_backends_ocaml    "llvm"                            "$llvm_targets"

for target in $llvm_targets; do
    touch "llvm_${target}.opam"
    create_dune_file "llvm_$target"       backends                   "llvm_$target"       "${target}_ocaml"     "llvm"                            "$target"
done
