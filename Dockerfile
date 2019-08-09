FROM haskell:8.6.5

COPY . .
RUN stack upgrade && stack --version
RUN stack update
# RUN stack build
RUN stack ghci
