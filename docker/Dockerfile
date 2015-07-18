# Base image
FROM heikomaass/android-sdk

# Author
MAINTAINER doctorq <542113578@qq.com>

# Upgrade apt-get tool
RUN apt-get update
RUN apt-get -y upgrade

# Install tools
RUN apt-get install -y git m4 zlib1g-dev python-software-properties \
    build-essential libgmp-dev libmpfr-dev libmpc-dev unzip software-properties-common python-software-properties



# Install opam
RUN add-apt-repository ppa:avsm/ppa
RUN apt-get update
RUN apt-get install -y ocaml ocaml-native-compilers camlp4-extra opam
RUN opam init --comp=4.01.0  # (answer 'y' to the question)
ENV CAML_LD_LIBRARY_PATH /root/.opam/4.01.0/lib/stublibs
ENV MANPATH :/root/.opam/4.01.0/man
ENV PERL5LIB /root/.opam/4.01.0/lib/perl5
ENV OCAML_TOPLEVEL_PATH /root/.opam/4.01.0/lib/toplevel
ENV OPAM_HOME /root/.opam/4.01.0/bin
ENV PATH ${PATH}:${CAML_LD_LIBRARY_PATH}:${MANPATH}:${PERL5LIB}:${OCAML_TOPLEVEL_PATH}:${OPAM_HOME}
#RUN opam update
RUN opam install sawja.1.5.1 atdgen.1.6.0 javalib.2.3.1 extlib.1.5.4


# Download infer source code
RUN git clone https://github.com/facebook/infer.git

# Install facebook-clang-plugin,clang
RUN cd infer;./update-fcp.sh;
RUN cd infer;apt-get install -y groff
RUN cd infer;../facebook-clang-plugin/clang/setup.sh;
RUN eval `opam config env`;cd infer;./compile-fcp.sh
RUN eval `opam config env`;cd infer;make -C infer
ENV INFER_HOME /infer/infer
ENV PATH ${PATH}:${INFER_HOME}/bin

#Install gradle-2.5
RUN add-apt-repository ppa:cwchien/gradle
RUN apt-get update
#RUN apt-cache search gradle
RUN apt-get install gradle-2.5
RUN cd infer/examples/android_hello;gradle
CMD eval `opam config env`