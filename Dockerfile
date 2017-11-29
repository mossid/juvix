FROM fpco/stack-build:lts-9.14

RUN mkdir /juvix
WORKDIR /juvix
ADD . /juvix
RUN stack build --install-ghc && stack install && stack clean --full
