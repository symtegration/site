---
title: On the implementation of integration
description: Notes on the implementation of integration in Symtegration, a Haskell library for symbolic integration.
author: 'Yoo Chung|https://chungyc.org/'
published: 2025-01-20
---

Symtegration can integrate certain mathematical expressions directly:

*   [Integration to well-known integrals](basic/)

*   [Integration of rational functions](rational/)

Symtegration also applies [general integration techniques](techniques/) on top of these
to expand the range of mathematical expressions it can integrate, including those which
include symbols that are not of the integrated variable.
You can see [some examples](/integral/) of such integrals.
This is all tied together in the
[Symtegration<wbr>.Integration](https://doc.symtegration.dev/symtegration/latest/Symtegration-Integration.html) module.

See also [more implementation notes on Symtegration](/note/implementation/).
