---
title: On the implementation of Symtegration
description: Reference to various notes on the implementation of Symtegration, which is a Haskell library for symbolic integration.
author: 'Yoo Chung|https://chungyc.org/'
published: 2025-01-16
updated: 2025-01-17
---

The following are various notes on what the implementation of [Symtegration] does.
In particular, the notes on what has been implemented for integration
generally maps to what Symtegration can integrate.

*   Integration

    *   [Basic integrals](integration/basic/)

    *   [General integration techniques](integration/techniques/)

The [package documentation] reflects the overall structure of the implementation.
In fact, almost all use cases only need to use the [Symtegration module].
All of the other modules are in support of it, and they only need to be used directly in special cases.

[Symtegration]: https://symtegration.dev/ {itemprop="about"}

[package documentation]: https://doc.symtegration.dev/symtegration/latest/

[Symtegration module]: https://doc.symtegration.dev/symtegration/latest/Symtegration.html
