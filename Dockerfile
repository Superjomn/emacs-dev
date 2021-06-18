FROM ubuntu:bionic
MAINTAINER Julio Delgado <julio.delgadomangas@gmail.com>
ENV USER root
ENV HOME /root

RUN apt-get update && apt-get install software-properties-common wget -y
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - && \
        apt-add-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main" && \
        apt-get update && \
        apt-get install -y clang-6.0

RUN echo deb http://ppa.launchpad.net/kelleyk/emacs/ubuntu bionic main >> /etc/apt/sources.list
RUN echo deb-src http://ppa.launchpad.net/kelleyk/emacs/ubuntu bionic main >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3FF0E01EEAAFC9CD

#RUN add-apt-repository ppa:kelleyk/emacs 

RUN apt-get update \
    && apt-get install -y curl \
                       file \
                       git \
                       emacs27 \
                       gcc \
                       g++ \
                       libclang-6.0-dev \
                       cmake \
                       python3-pip \
                       libboost-all-dev \
                       zlib1g-dev


# RTAGS
RUN mkdir -p /opt/src && cd /opt/src/ && git clone --recursive https://github.com/Andersbakken/rtags.git && \
    cd rtags && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 . && make install -j2 && cd .. && rm -rf rtags
#ENV PATH "${PATH}:/opt/src/rtags/bin/"

# the DOOM emacs depends the latest git.
RUN echo deb http://ppa.launchpad.net/git-core/ppa/ubuntu bionic main >> /etc/apt/sources.list && \
    echo deb-src http://ppa.launchpad.net/git-core/ppa/ubuntu bionic main >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys A1715D88E1DF1F24 && \
    apt update
#RUN add-apt-repository ppa:git-core/ppa && 
RUN apt install -y git
# use a specific commit to avoid doom-emacs broken update
RUN git clone https://github.com/hlissner/doom-emacs ~/.emacs.d && cd ~/.emacs.d && git checkout 2731685
RUN printf 'y\ny' | ~/.emacs.d/bin/doom -y install

# update doom config
RUN git clone https://github.com/Superjomn/emacs-dev.git && echo 0 && cp emacs-dev/.doom.d/* ~/.doom.d/ && ~/.emacs.d/bin/doom sync
# fix irony missing file error
RUN mkdir -p /root/.emacs.d/.local/etc/irony
RUN touch /root/.emacs.d/.local/etc/irony/cdb-json-projects
