## Dockerfile for a haskell environment in CentOS 6
## Inspired by https://github.com/dimchansky/docker-centos6-haskell/blob/master/7.8.4/Dockerfile
FROM       centos:7
MAINTAINER Arnaud Bailly <arnaud@igitur.io>

## Install dependencies
RUN yum install -y curl\
    gcc \
    gmp-devel \
    pcre-devel \
    perl \
    tar \
    which \
    xz \
    zlib-devel \
    && yum clean all --releasever=6 \
    && ln -s /lib64/libtinfo.so.5 /lib64/libtinfo.so

ENV GHC_VERSION=7.10.2

RUN yum install -y make

## Install stack
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'
ENV PATH /opt/stack/bin/:$PATH
RUN stack setup $GHC_VERSION

# application specific stuff
RUN yum install -y lapack-devel blas-devel
