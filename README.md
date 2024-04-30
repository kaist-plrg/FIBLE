## Getting Started

## Prerequisites

- Linux
```sh
apt install curl make gcc unzip bubblewrap libffi-dev libgmp-dev pkg-config
```

- MacOS
```sh
brew install libffi-dev gmp pkg-config
```


## Build

```sh
$ ./install.sh
$ eval $(opam env)
$ dune build
```