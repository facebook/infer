FROM debian:stretch-slim

LABEL maintainer "Infer team"

# mkdir the man/man1 directory due to Debian bug #863199
RUN apt-get update && \
    mkdir -p /usr/share/man/man1 && \
    apt-get install --yes --no-install-recommends \
      autoconf \
      automake \
      bzip2 \
      cmake \
      curl \
      gcc \
      git \
      libc6-dev \
      libgmp-dev \
      libmpfr-dev \
      libsqlite3-dev \
      make \
      openjdk-8-jdk-headless \
      patch \
      pkg-config \
      python2.7 \
      unzip \
      zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

# Some scripts in facebook-clang-plugins assume "python" is available
RUN cd /usr/local/bin && ln -s /usr/bin/python2.7 python

# Install opam 2
RUN curl -sL https://github.com/ocaml/opam/releases/download/2.0.2/opam-2.0.2-x86_64-linux > /usr/bin/opam && \
    chmod +x /usr/bin/opam

# Disable sandboxing
# Without this opam fails to compile OCaml for some reason. We don't need sandboxing inside a Docker container anyway.
RUN opam init --reinit --bare --disable-sandboxing

# Download the latest Infer master
RUN cd / && \
    git clone https://github.com/facebook/infer/

# Build opam deps first, then infer. This way if any step fails we
# don't lose the significant amount of work done in the previous
# steps.
RUN cd /infer && \
    INFER_OPAM_SWITCH=4.07.1 ./build-infer.sh --only-setup-opam --no-opam-lock java && \
    opam clean

# Make sure clang is disabled
RUN eval $(opam env) && \
    cd /infer && \
    SKIP_SUBMODULES=true ./autogen.sh && \
    ./configure --disable-c-analyzers

# "Install" Infer. The tutorial assumes /infer-host is the working copy of infer so let's put it in the PATH too.
ENV PATH /infer-host/infer/bin:/infer/bin:$PATH

# build in non-optimized mode by default to speed up build times
ENV BUILD_MODE=default

# prevent exiting by compulsively hitting Control-D
ENV IGNOREEOF=9

# should be moved earlier
ENV INFER_OPAM_SWITCH=4.07.1

# export `opam env`
ENV OPAM_SWITCH_PREFIX=/root/.opam/4.07.1
ENV CAML_LD_LIBRARY_PATH=/root/.opam/4.07.1/lib/stublibs:/root/.opam/4.07.1/lib/ocaml/stublibs:/root/.opam/4.07.1/lib/ocaml
ENV OCAML_TOPLEVEL_PATH=/root/.opam/4.07.1/lib/toplevel
ENV MANPATH=$MANPATH:/root/.opam/4.07.1/man
ENV PATH=/root/.opam/4.07.1/bin:$PATH
