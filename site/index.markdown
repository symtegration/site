---
front: true
title: Symtegration
description: Symtegration is a Haskell library for symbolic integration.
---

Symtegration is a Haskell library intended to support symbolic integration of mathematical expressions.

```haskell
>>> :load Symtegration
>>> toHaskell <$> integrate "x" (4 * "x" **3 + 1)
Just "x + (x ** 4)"
>>> toHaskell <$> integrate "x" (1 / (1 + "x" ** 2))
Just "atan x"
```

## Project information {#information}

*   [Source repository](https://github.com/symtegration/symtegration)

*   Primary maintainer

    *   [Yoo Chung](https://chungyc.org) &mdash; [`@chungyc`](https://github.com/chungyc) on GitHub
