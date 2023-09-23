# Rinha


## Como rodar o projeto

Faça o build com:

```bash
docker build . -t="rinha"
```

Rode os examplos com:


```bash
docker run rinha files/fib.json
```


Rode outros examples por bind volume usando:

```bash
docker run -v $(pwd)/files/fib.json:/var/rinha/source.rinha.json rinha
```

## Dev Setup with Nix

```bash
nix develop -c $SHELL
```


## Como rodar os testes

```bash
dune build @runtest --force --no-buffer
```
