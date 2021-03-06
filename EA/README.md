# EA

[EA](https://en.xen.wiki/w/Intro_to_exterior_algebra_for_RTT) utilities, implemented
in [Wolfram Language](https://www.wolfram.com/language/) (formerly Mathematica), a popular and capable programming
language for working with math.

This file relies on the modules in `main.m`. You will need to add that file to scope, and then you will be able to use
this one.

This file contains the following functions:

* `eaGetD`
* `eaGetR`
* `eaGetN`
* `eaCanonicalForm`
* `eaDual`
* `multivectorToMatrix`
* `matrixToMultivector`
* `progressiveProduct`
* `regressiveProduct`
* `interiorProduct`
* `eaSum`
* `eaDiff`

It is based on material from the following article:

* [Dave Keenan & Douglas Blumeyer's guide to EA for RTT](https://en.xen.wiki/w/Dave_Keenan_&_Douglas_Blumeyer's_guide_to_EA_for_RTT)

## data structures

Multivectors are implemented in this library as ordered triplets:

1. the list of largest-minors
2. the grade (the count of brackets)
3. the variance (whether the brackets point to the left or the right)

In the case of nilovectors, a fourth entry is required in order to fully specify the temperament: the dimensionality.

All multivectors in this library are varianced. So "multivector" refers to multivectors that may be of either variance,
and "contravariant multivector" and "covariant multivector" are used for the specific variances.

Examples:

* meantone's multimap (wedgie) ⟨⟨1 4 4]] is input as `{{1, 4, 4}, 2, "co"}`
* meantone's multicomma [4 -4 1⟩ is input as `{{4, -4, 1}, 1, "contra"}`

Recognized variance strings for covariant multivectors:

* `"co"`
* `"covector"`
* `"multicovector"`
* `"covariant"`
* `"m"`
* `"map"`
* `"multimap"`
* `"val"`
* `"multival"`
* `"with"`
* `"mm"`

Recognized variance strings for contravariant multivectors:

* `"contra"`
* `"contravector"`
* `"multicontravector"`
* `"contravariant"`
* `"v"`
* `"vector"`
* `"c"`
* `"comma"`
* `"multicomma"`
* `"i"`
* `"interval"`
* `"multinterval"`
* `"multiinterval"`
* `"monzo"`
* `"multimonzo"`
* `"against"`
* `"wedgie"`
* `"mc"`

## edge cases

Note that while nilovectors are essentially scalars, their first entry is still technically a largestMinorsL *list*,
albeit one with a single entry. So for example, the scalar `5` is input as `{{5}, 0, v, d}`. This indicates the number 5
nested inside zero brackets. The braces around the first element do not necessarily mean that the object represented has
brackets.

## conventional single-character (or double-character) variable names

### multivectors

* `u = {largestMinorsL, variance, grade, d}`: temperament, represented as a multivector
* `mm`: multimap, a covariant `u`
* `mc`: multicomma, a contravariant `u`

## roadmap

The following features are planned:

* IO
    * EBK notation
    * matrix display
* error handling
    * progressive product across different dimensionalities
    * minors lists not matching grade
