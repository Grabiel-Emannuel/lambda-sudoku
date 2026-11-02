FROM haskell:9.10.3

ARG THREADS=1
ENV THREADS=${THREADS}
ENV DEPTH=1
ENV SOLUTIONS=100

WORKDIR /opt/lambda-sudoku

COPY lambda-sudoku.cabal ./
COPY app ./app

RUN cabal update \
    && cabal build exe:lambda-sudoku --jobs="${THREADS}" \
    && install "$(cabal list-bin exe:lambda-sudoku)" /usr/local/bin/lambda-sudoku

COPY docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
RUN chmod +x /usr/local/bin/docker-entrypoint.sh

WORKDIR /data
ENTRYPOINT ["docker-entrypoint.sh"]
