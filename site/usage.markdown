---
title: How to use Symtegration
description: Instructions on how to use Symtegration, a Haskell library for symbolic integration.
---

## Installation {#install}

For using the Symtegration library locally, clone the source repository, build, and install it.

```bash
$ git clone git@github.com:symtegration/symtegration.git
$ cd symtegration
$ stack build
$ stack install
```

You can then use Symtegration in [GHCi](#ghci) or local Haskell projects.
If you use [Cabal] instead of [Stack], then you can run `cabal build` and `cabal install`, instead.

[Cabal]: https://www.haskell.org/cabal/

[Stack]: https://docs.haskellstack.org/

To use as a library in a Haskell Stack project, at the following to the `extra-deps` field in `stack.yaml`,
and add `symtegration` as a dependency to the appropriate build targets.

```yaml
extra-deps:
- git: https://github.com/symtegration/symtegration
  commit: '7d49eca9f48adf6740dcb3a7934872e3b91a30e6'
```

See the [library documentation] for more details as to how to use the library.

[library documentation]: https://doc.symtegration.dev/symtegration-0.3.0/Symtegration.html

## GHCi {#ghci}

With Symtegration, symbolic integration can be done within [GHCi].
When executing GHCi within the Symtegration project, it is best
to load only the `Symtegration` module to avoid name collisions,
so start GHCi without loading any modules.

```bash
$ stack ghci --no-load
```

Within GHCi, explicitly load the `Symtegration` module.
You can then proceed to symbolically integrate mathematical expressions
and compute approximate or exact values from these integrals.
Mathematical expressions are entered as normal Haskell expressions,
where symbols can be entered as strings when the [`OverloadedStrings`] extension is enabled.

```haskell
>>> :load Symtegration
>>> toHaskell <$> integrate "x" ("a" * "x" ** 4 + "x" + "b")
Just "b * x + (1 / 2) * (x ** 2) + a * ((x ** 5) / 5)"
>>>
>>> let (Just p) = integrate "x" ("x" ** 2)
>>> evaluate p (\case "x" -> Just 1)
Just 0.3333333333333333
>>>
>>> fractionalEvaluate p (\case "x" -> Just (1 :: Rational))
Just (1 % 3)
```

[GHCi]: https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html

[`OverloadedStrings`]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html

## IHaskell {#ihaskell}

Symtegration can also be used in [IHaskell] to do symbolic integration.
Its use can be seen in an [example IHaskell notebook],
which you try out by [running on mybinder.org].

[IHaskell]: https://github.com/IHaskell/IHaskell

[example IHaskell notebook]: https://github.com/chungyc/haskell-notebooks/blob/main/Symtegration.ipynb

[running on mybinder.org]: https://mybinder.org/v2/gh/chungyc/ihaskell/custom?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252Fchungyc%252Fhaskell-notebooks%26urlpath%3Dlab%252Ftree%252Fhaskell-notebooks%252FSymtegration.ipynb%26branch%3Dmain
