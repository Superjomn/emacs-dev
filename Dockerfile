FROM ubuntu:bionic
MAINTAINER Julio Delgado <julio.delgadomangas@gmail.com>
ENV USER root
ENV HOME /root

RUN apt-get update && apt-get install software-properties-common wget -y
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - && \
        apt-add-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main" && \
        apt-get update && \
        apt-get install -y clang-6.0

RUN add-apt-repository ppa:kelleyk/emacs && apt-get update \
    && apt-get install -y curl \
                       file \
                       git \
                       emacs26 \
                       gcc \
                       g++ \
                       libclang-6.0-dev \
                       cmake \
                       python3-pip \
                       libboost-all-dev \
                       zlib1g-dev


# RTAGS
RUN mkdir -p /opt/src && cd /opt/src/ && git clone --recursive https://github.com/Andersbakken/rtags.git && \
    cd rtags && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 . && make -j2
ENV PATH "${PATH}:/opt/src/rtags/bin/"

# the DOOM emacs depends the latest git.
RUN add-apt-repository ppa:git-core/ppa && apt install -y git
RUN git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
RUN printf 'y\ny' | ~/.emacs.d/bin/doom -y install
RUN ~/.emacs.d/bin/doom sync
