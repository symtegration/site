---
front: true
title: Symtegration
description: Symtegration is a Haskell library for symbolic integration.
---

Symtegration is a Haskell library intended to support symbolic integration of mathematical expressions.

```haskell
>>> import Symtegration
>>> toHaskell <$> integrate "x" (4 * "x" **3 + 1)
Just "x + x ** 4"
>>> toHaskell <$> integrate "x" (1 / (1 + "x" ** 2))
Just "atan x"
```

For more information, see [how to use Symtegration] and [examples] of functions which can be integrated by Symtegration.

[how to use Symtegration]: /usage/
[examples]: /integral/
