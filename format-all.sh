#!/bin/sh
cabal-gild --input lambda/lambda.cabal --output lambda/lambda.cabal
cabal-gild --input shell/shell.cabal --output shell/shell.cabal
fourmolu -i lambda/src shell/src
