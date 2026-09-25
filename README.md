
## Step 1

```
$ nix develop
$ cabal repl shell
> Just term <- Shell.parseFile "examples/09-paraboloid.hs"
> let r = Lambda.reduceFull term
> Codec.decodeNat r
Just (Nat 272)
```

## Step 2

```
$ nix develop
$ cabal repl shell
> Just term2 <- Shell.parseFile "examples/10-alpha.hs"
> let r2 = Lambda.reduceFull term2
> putStrLn $ Printer.toString r2
\x -> \x1 -> x x1
```

## Step 3

```
$ git checkout step3
```
