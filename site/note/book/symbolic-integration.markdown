---
title: 'Implementor notes for Symbolic Integration I: Transcendental Functions'
description: 'Notes which may be helpful for implementors basing their work on the book Symbolic Integration I: Transcendental Functions by Manuel Bronstein.'
author: 'Yoo Chung|https://chungyc.org/'
published: 2025-01-16
---

These are notes which may be helpful for implementors who wish to implement the algorithms in
<span itemprop="citation">_[Symbolic Integration I: Transcendental Functions]_ by Manuel Bronstein</span>.
These have been noted during the implementation of various algorithms in [Symtegration].

[Symbolic Integration I: Transcendental Functions]: https://link.springer.com/book/10.1007/b138171 {itemprop="about"}

[Symtegration]: https://symtegration.dev

## Polynomial pseudo-division {#poynomial-pseudodivide}

In section 1.2, page 9, the algorithm for polynomial pseudo-division does not apply
when the degree of \(A\) is already smaller than \(B\).

## Greatest common divisor {#gcd}

Section 1.3 gives algorithms which derives the greatest common divisor for two polynomials.
It was noted in theorem 1.1.1 in section 1.1 that greatest common divisors for polynomials
are unique up to multiplication by units.  For polynomials where the coefficients are a field,
all non-zero constants are units, and the book uses the convention of using the monic greatest
common divisor, i.e., the greatest common divisor polynomial where the coefficients have been
scaled so that the leading coefficient is 1, in many of the algorithms.

For example, the Hermite reduction algorithm in section 2.2 on page 44 may result in a spurious
constant factor multiplying the remaining rational function \(h\) to be integrated when using
the greatest common divisor computed by the Euclidean algorithms in section 1.3 directly.
The greatest common divisor must be made monic for the Hermite reduction to give correct results.

## Resultant {#resultant}

Theorem 1.4.1 in section 1.4, page 18, notes that for two polynomials
\(A = a \prod_{i=1}^{n} (x - \alpha_i)\) and \(B = b \prod_{i=1}^{m} (x - \beta_i)\),
the resultant is

\[ R = a^m b^n \prod_{i=1}^n \prod_{j=1}^m (\alpha_i - \beta_j) \]

Note that it is \(a^m b^n\) in the above.  It is _not_ \(a^n b^m\).
The exponent for \(a\) is the number of roots for \(B\),
and the exponent for \(b\) is the number of roots for \(A\).
Take care not to switch the exponents around.  It can be difficult to
notice the mistake if this happens if one is not aware of the possibility.
