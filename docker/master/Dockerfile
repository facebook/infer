FROM debian:bullseye-slim AS compilator

LABEL maintainer "Infer team"

# mkdir the man/man1 directory due to Debian bug #863199
RUN apt-get update && \
    mkdir -p /usr/share/man/man1 && \
    apt-get install --yes --no-install-recommends \
      autoconf \
      automake \
      bubblewrap \
      bzip2 \
      cmake \
      curl \
      g++ \
      gcc \
      git \
      libc6-dev \
      libgmp-dev \
      libmpfr-dev \
      libsqlite3-dev \
      sqlite3 \
      make \
      opam \
      openjdk-11-jdk-headless \
      patch \
      patchelf \
      pkg-config \
      python3 \
      python3-distutils \
      unzip \
      xz-utils \
      zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

# Disable sandboxing
# Without this opam fails to compile OCaml for some reason. We don't need sandboxing inside a Docker container anyway.
RUN opam init --reinit --bare --disable-sandboxing --yes --auto-setup

# Download the latest Infer from git
RUN cd / && \
    git clone --depth 1 https://github.com/facebook/infer/

# Build opam deps first, then clang, then infer. This way if any step
# fails we don't lose the significant amount of work done in the
# previous steps.
RUN cd /infer && ./build-infer.sh --only-setup-opam
RUN cd /infer && \
    eval $(opam env) && \
    ./autogen.sh && \
    ./configure && \
    ./facebook-clang-plugins/clang/setup.sh

# Generate a release
RUN cd /infer && \
    make install-with-libs \
    BUILD_MODE=opt \
    PATCHELF=patchelf \
    DESTDIR="/infer-release" \
    libdir_relative_to_bindir="../lib"

FROM debian:bullseye-slim AS executor

RUN apt-get update && apt-get install --yes --no-install-recommends sqlite3

# Get the infer release
COPY --from=compilator /infer-release/usr/local /infer

# Install infer
ENV PATH /infer/bin:${PATH}

# if called with /infer-host mounted then copy infer there
RUN if test -d /infer-host; then \
      cp -av /infer/. /infer-host; \
    fi
