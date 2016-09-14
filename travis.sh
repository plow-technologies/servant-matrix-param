#!/usr/bin/env bash

set -o errexit

stack setup --no-terminal
stack build --ghc-options=-Werror --no-terminal
stack test  --ghc-options=-Werror --no-terminal
