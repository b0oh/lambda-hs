
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
$ nix develop
$ cabal repl shell
> Just term3 <- Shell.parseFile "examples/11-pair.hs"
> let n = Lambda.normalize term3
> Codec.decode n
Just (Pair (Nat 2) (Nat 4))
> Just term4 <- Shell.parseFile "examples/12-combs.hs"
> let n2 = Lambda.normalize term4
> Codec.decode n2
Just (Pair (Bool False) (Bool True))
> Just term5 <- Shell.parseFile "examples/13-decrement.hs"
> let n3 = Lambda.normalize term5
> Codec.decode n3
Just (Nat 255)
> Just term6 <- Shell.parseFile "examples/14-arith.hs"
> let n4 = Lambda.normalize term6
> Codec.decode n4
Just (Nat 250)
> Just term7 <- Shell.parseFile "examples/15-factorial.hs"
> let n5 = Lambda.normalize term7
> Codec.decode n5
Just (Nat 24)
```

## Step 4

```
$ git checkout step4
```
