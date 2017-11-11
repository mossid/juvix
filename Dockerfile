FROM fpco/stack-build:lts-9.10

RUN mkdir /juvix
WORKDIR /juvix
ADD . /juvix
RUN stack build --install-ghc && stack install && stack clean --full
