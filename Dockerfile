FROM ubuntu:16.04

ENV STACK_VERSION 1.5.1
ENV STACK_DOWNLOAD_URL https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz
ENV PATH $PATH:/root/.local/bin

RUN apt-get update -q && \
    apt-get install -qy libgmp-dev libnss3-dev ca-certificates netbase wget xz-utils build-essential libncurses-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /root/.local/bin && \
    wget -q -O- $STACK_DOWNLOAD_URL | tar --strip=1 -xvz -C /root/.local/bin/ && \
    chmod +x /root/.local/bin/stack

RUN mkdir /juvix
WORKDIR /juvix
ADD . /juvix
RUN stack build --install-ghc && stack install && stack clean --full
