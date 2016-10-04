#!/usr/bin/env bash

set -o errexit

stack setup --no-terminal
stack test  --ghc-options=-Werror --no-terminal --flag servant-matrix-param:with-servant-aeson-specs  --flag servant-matrix-param:with-servant-server
stack build --ghc-options=-Werror --no-terminal --flag servant-matrix-param:with-servant-aeson-specs  --flag servant-matrix-param:-with-servant-server
stack build --ghc-options=-Werror --no-terminal --flag servant-matrix-param:-with-servant-aeson-specs --flag servant-matrix-param:with-servant-server
stack build --ghc-options=-Werror --no-terminal --flag servant-matrix-param:-with-servant-aeson-specs --flag servant-matrix-param:-with-servant-server

stack test  --ghc-options=-Werror --no-terminal --flag --stack-yaml=stack-servant-0.7.yaml
