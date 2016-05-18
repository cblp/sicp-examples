## Prepare Environment

### OS

Ubuntu 16.04. No other variants supported yet.

### Haskell

    $ sudo apt install haskell-stack
    $ stack setup

### OCaml

    $ sudo apt install ocaml-interp

### Rust

TODO(cblp, 2016-05-18)

### Go

TODO(cblp, 2016-05-18)

<!-- _Note:_ `rusti` currently supports nightly Rust only.

    $ sudo apt install cargo
    $ ./rustup.sh --channel=nightly --prefix=~/.local
    $ cargo install --git https://github.com/murarth/rusti -->

## Run Examples

    $ stack test
