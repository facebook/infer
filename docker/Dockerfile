# Base image
FROM buildpack-deps:xenial-curl

MAINTAINER Infer

# Debian config
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
            autoconf \
            automake \
            curl \
            gcc \
            g++ \
            git \
            groff \
            libc6-dev \
            libffi-dev \
            libgmp-dev \
            libmpc-dev \
            libmpfr-dev \
            m4 \
            make \
            ncurses-dev \
            ocaml \
            openjdk-8-jdk-headless \
            pkg-config \
            python-software-properties \
            rsync \
            software-properties-common \
            unzip \
            zlib1g-dev

# Install OPAM
RUN VERSION=1.2.2; \
    curl -sL \
      https://github.com/ocaml/opam/releases/download/$VERSION/opam-$VERSION-x86_64-Linux \
      -o /usr/local/bin/opam && \
    chmod 755 /usr/local/bin/opam && \
    ((/usr/local/bin/opam --version | grep -q $VERSION) || \
     (echo "failed to download opam from GitHub."; exit 1))
RUN opam init -y --comp=4.02.3

# Download the latest Infer release
RUN INFER_VERSION=v0.10.0; \
    cd /opt && \
    curl -sL \
      https://github.com/facebook/infer/releases/download/${INFER_VERSION}/infer-linux64-${INFER_VERSION}.tar.xz | \
    tar xJ && \
    rm -f /infer && \
    ln -s ${PWD}/infer-linux64-$INFER_VERSION /infer

# Compile Infer
RUN cd /infer && \
    ./build-infer.sh

# Install Infer
ENV INFER_HOME /infer/infer
ENV PATH ${INFER_HOME}/bin:${PATH}

# Install dependencies for Android sample
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
            libc6-dev \
            libc6-i386 \
            lib32z1 \
            lib32stdc++6 && \
    rm -rf /var/lib/apt/lists/*

ENV ANDROID_HOME /opt/android-sdk-linux
WORKDIR $ANDROID_HOME
RUN curl -o sdk-tools-linux.zip \
      https://dl.google.com/android/repository/sdk-tools-linux-3859397.zip && \
    unzip sdk-tools-linux.zip && \
    rm sdk-tools-linux.zip
ENV PATH ${ANDROID_HOME}/tools/bin:${PATH}
RUN echo "sdk.dir=${ANDROID_HOME}" > /infer/examples/android_hello/local.properties
