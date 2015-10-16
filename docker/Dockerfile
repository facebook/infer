# Base image
FROM heikomaass/android-sdk

# Author
MAINTAINER doctorq <542113578@qq.com>


# Install tools
RUN apt-get update; \
    apt-get upgrade; \
    apt-get install -y \
            build-essential \
            git \
            groff \
            libgmp-dev \
            libmpc-dev \
            libmpfr-dev \
            m4 \
            python-software-properties \
            software-properties-common \
            unzip \
            zlib1g-dev

# Install OPAM
RUN add-apt-repository ppa:avsm/ppa
RUN apt-get update; \
    apt-get install -y \
            camlp4-extra \
            ocaml \
            ocaml-native-compilers \
            opam
RUN opam init --yes --comp=4.01.0
RUN opam install -y extlib.1.5.4 atdgen.1.6.0 javalib.2.3.1 sawja.1.5.1

# Download Infer source code
RUN git clone https://github.com/facebook/infer.git

# Install Facebook Clang Plugins and clang
RUN cd infer; git pull; \
    git submodule update --init; \
    facebook-clang-plugins/clang/setup.sh
RUN cd infer; eval $(opam config env); \
    git submodule update; \
    ./compile-fcp.sh

# Install Infer
RUN cd infer; eval $(opam config env); \
    make -C infer clang java
ENV INFER_HOME /infer/infer
ENV PATH ${INFER_HOME}/bin:${PATH}

# Install Gradle 2.5
RUN add-apt-repository ppa:cwchien/gradle
RUN apt-get update; apt-get install gradle-2.5
