# Use a base image with OCaml pre-installed
FROM ocaml/opam:alpine-ocaml-5.1-flambda AS BUILD
RUN sudo apk update && sudo apk upgrade

# Set a working directory
WORKDIR /app

# Copy the opam files into the container for cache purposes
COPY rinha.opam .

# Install opam dependencies
RUN opam install . --deps-only --locked

# Copy OCaml code into the container
COPY . /app

# Build OCaml application
RUN eval $(opam env) && sudo dune build src/main.exe
RUN sudo cp ./_build/default/src/main.exe /usr/bin/main.exe
RUN sudo cp -rf ./files /usr/bin/files


# Run OCaml application 
FROM alpine AS APP
COPY --from=BUILD /usr/bin/main.exe /home/app/rinha.exe
COPY --from=BUILD /usr/bin/files /home/app/files
RUN apk update && apk upgrade
RUN adduser -D app
RUN chown app:app /home/app/rinha.exe
WORKDIR /home/app
USER app
ENTRYPOINT ["/home/app/rinha.exe"]
CMD ["/var/rinha/source.rinha.json"]
