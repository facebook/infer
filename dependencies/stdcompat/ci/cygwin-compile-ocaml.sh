#!/bin/bash
set -ex
OCAMLVERSION="$1"
URL="http://caml.inria.fr/pub/distrib/ocaml-${OCAMLVERSION:0:4}/ocaml-$OCAMLVERSION.tar.gz"
if [ ! -d "/cygdrive/c/ocaml/$OCAMLVERSION/" ]; then
    PREFIX="C:/ocaml/$OCAMLVERSION"
    SOURCEDIR="ocaml-$OCAMLVERSION"
    if [ ! -d "$SOURCEDIR" ]; then
      TARBALL="ocaml-$OCAMLVERSION.tar.gz"
      if [ ! -f "$TARBALL" ]; then
        wget "$URL"
      fi
      tar --extract --file=$TARBALL
    fi
    cd "ocaml-$OCAMLVERSION"
    if [ `printf "$OCAMLVERSION\n4.03.0" | sort | head -n1` = 4.03.0 ]; then
        eval $(tools/msvs-promote-path)
        pushd flexdll
            wget https://github.com/alainfrisch/flexdll/archive/0.37.tar.gz
            tar --strip-components=1 --extract --file=0.37.tar.gz
            popd
    else
        eval $(~/ocaml-4.09.0/tools/msvs-promote-path)
        export PATH="$HOME/ocaml-4.09.0/flexdll:$PATH"
        export INCLUDE="$INCLUDE;C:/Users/ci/ocaml-4.09.0/flexdll"
        export OCAMLBUILD_FIND=/usr/bin/find
    fi
    if [ `printf "$OCAMLVERSION\n4.08.0" | sort | head -n1` = 4.08.0 ]; then
        ./configure --build=x86_64-unknown-cygwin --host=x86_64-pc-windows \
            --prefix="$PREFIX"
    else
        if [ `printf "$OCAMLVERSION\n4.06.0" | sort | head -n1` = 4.06.0 ]; then
            cp config/m-nt.h byterun/caml/m.h
            cp config/s-nt.h byterun/caml/s.h
        else
            cp config/m-nt.h config/m.h
            cp config/s-nt.h config/s.h
        fi
        cp config/Makefile.msvc64 config/Makefile
        sed -i -e "s|^PREFIX=.*\$|PREFIX=$PREFIX|" config/Makefile
    fi
    if [ `printf "$OCAMLVERSION\n4.05.0" | sort | head -n1` = 4.05.0 ]; then
        make flexdll
        make world.opt
        make flexlink.opt
        make install
    elif [ `printf "$OCAMLVERSION\n4.03.0" | sort | head -n1` = 4.03.0 ]; then
        make -f Makefile.nt flexdll world bootstrap opt opt.opt install
    else
        make -f Makefile.nt world bootstrap opt opt.opt install
    fi
fi
