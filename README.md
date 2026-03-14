
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
$ git checkout step2
```
