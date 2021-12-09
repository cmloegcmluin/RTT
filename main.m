(* TEMPERAMENT UTILITIES *)

(*

getD[t]

Given a representation of a temperament as a mapping or comma basis,
returns the dimensionality.

Examples:

In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
      getD[meantoneM]

Out   3

In    meantoneC = {{{4, -4, 1}}, "contra"};
      getD[meantoneC]

Out   3

*)
getD[t_] := colCount[getA[t]];

(*

getR[t]

Given a representation of a temperament as a mapping or comma basis,
returns the rank.

Examples:

In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
      getR[meantoneM]

Out   2

In    meantoneC = {{{4, -4, 1}}, "contra"};
      getR[meantoneC]

Out   2

*)
getR[t_] := If[
  isCo[t],
  MatrixRank[getA[t]],
  getD[t] - MatrixRank[getA[t]]
];

(*

getN[t]

Given a representation of a temperament as a mapping or comma basis,
returns the nullity.

Examples:

In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
      getN[meantoneM]

Out   1

In    meantoneC = {{{4, -4, 1}}, "contra"};
      getN[meantoneC]

Out   1

*)
getN[t_] := If[
  isContra[t],
  MatrixRank[getA[t]],
  getD[t] - MatrixRank[getA[t]]
];


(* CANONICALIZATION *)

(*

canonicalForm[t]

Returns the given temperament representation (mapping or comma basis)
in canonical form (defactored, then put into Hermite Normal Form).

Examples:

In    someMeantoneM = {{{5, 8, 12}, {7, 11, 16}}, "co"};
      canonicalForm[someMeantoneM]

Out   {{{1, 0, -4}, {0, 1, 4}}, "co"}

In    someMeantoneC = {{{-8, 8, -2}}, "contra"};
      canonicalForm[someMeantoneC]

Out   {{{4, -4, 1}, "contra"}

*)
canonicalForm[t_] := If[
  isContra[t],
  {canonicalC[getA[t]], getV[t]},
  {canonicalM[getA[t]], getV[t]}
];


(* DUAL *)

(*

dual[t]

Returns its dual for the given temperament representation
(if given a mapping, the comma basis, or vice-versa).

Examples:

In    meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
      dual[meantoneM]

Out   {{{4, -4, 1}}, "contra"}

*)
dual[t_] := If[
  isContra[t],
  {antiNullSpaceBasis[getA[t]], "co"},
  {nullSpaceBasis[getA[t]], "contra"}
];


(* MEET AND JOIN *)

(*

join[t1, t2, t3...]

Joins the given temperaments: concatenates their mappings
and puts the result into canonical form.

Can accept any number of temperaments representations,
as any combination of mappings or comma bases,
but returns the temperament as a mapping.

Examples:

In    et5 = {{{5, 8, 12}}, "co"};
      et7 = {{{7, 11, 16}}, "co"};
      join[et5, et7]

Out   {{{1, 0, -4}, {0, 1, 4}}, "co"};

In    et7d = {{{7, 11, 16, 19}}, "co"};
      et12 = {{{12, 19, 28, 34}}, "co"};
      et22 = {{{22, 35, 51, 62}}, "co"};
      join[et7dLimit7, et12Limit7, et22Limit7]

Out   {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"};

*)
join[tSequence___] := canonicalForm[{Apply[Join, Map[getM, {tSequence}]], "co"}];

(*

meet[t1, t2, t3...]

Meets the given temperaments: concatenates their comma bases
and puts the result into canonical form.

Can accept any number of temperament representations,
as any combination of mappings or comma bases,
but returns the temperament as a comma basis.

In    meantone = {{{4, -4, 1}}, "contra"};
      porcupine = {{{1, -5, 3}}, "contra"};
      meet[meantone, porcupine]

Out   {{{-11, 7, 0}, {-7, 3, 1}}, "contra"}

In    mint = {{{2, 2, -1, -1}}, "contra"};
      meantone = {{{4, -4, 1, 0}}, "contra"};
      negri = {{{-14, 3, 4, 0}}, "contra"};
      meet[mint, meantone, negri]

Out   {{{30, 19, 0, 0}, {-26, 15, 1, 0}, {-6, 2, 0, 1}}, "contra"}

*)
meet[tSequence___] := canonicalForm[{Apply[Join, Map[getC, {tSequence}]], "contra"}];




(* ___ PRIVATE ___ *)



(* LIST UTILITIES *)

divideOutGcd[l_] := Module[{gcd}, gcd = Apply[GCD, l]; If[gcd==0, l, l/gcd]];
multByLcd[l_] := Apply[LCM, Denominator[l]] * l;

leadingEntry[l_] := First[Select[l, # != 0&, 1]];
trailingEntry[l_] := leadingEntry[Reverse[l]];

allZerosL[a_] := AllTrue[a, # == 0&];


(* MATRIX UTILITIES *)

allZeros[a_] := AllTrue[a, # == 0&, 2];

reverseEachRow[a_] := Reverse[a, 2];
reverseEachCol[a_] := Reverse[a];
antiTranspose[a_] := reverseEachRow[reverseEachCol[a]];

removeAllZeroRows[a_] := Select[a, FreeQ[#, {0 ..}] &];

removeUnneededZeroRows[a_] := If[
  allZeros[a],
  {Table[0, colCount[a]]},
  removeAllZeroRows[a]
];

colCount[a_] := Last[Dimensions[a]];


(* TEMPERAMENT UTILITIES *)

getA[t_] := First[t];
getV[t_] := Last[t];

isContra[t_] := MemberQ[{
  "contra",
  "contravector",
  "contravariant",
  "v",
  "vector",
  "c",
  "comma",
  "comma basis",
  "comma-basis",
  "commaBasis",
  "comma_basis",
  "i",
  "interval",
  "g",
  "generator",
  "pcv",
  "gcv",
  "monzo",
  "against"
}, getV[t]];
isCo[t_] := MemberQ[{
  "co",
  "covector",
  "covariant",
  "m",
  "map",
  "mapping",
  "et",
  "edo",
  "edomapping",
  "val",
  "with"
}, getV[t]];


(* CANONICALIZATION *)

hnf[a_] := Last[HermiteDecomposition[a]];

hermiteRightUnimodular[a_] := Transpose[First[HermiteDecomposition[Transpose[a]]]];
colHermiteDefactor[a_] := Take[Inverse[hermiteRightUnimodular[a]], MatrixRank[a]];

canonicalM[m_] := If[
  allZeros[m],
  {Table[0, colCount[m]]},
  removeUnneededZeroRows[hnf[colHermiteDefactor[m]]]
];
canonicalC[c_] := antiTranspose[canonicalM[antiTranspose[c]]];


(* DUAL *)

noncanonicalNullSpaceBasis[m_] := reverseEachCol[NullSpace[m]];
noncanonicalAntiNullSpaceBasis[c_] := NullSpace[c];

nullSpaceBasis[m_] := Module[{c},
  c = canonicalC[noncanonicalNullSpaceBasis[m]];

  If[
    c == {{}},
    {Table[0, getD[m]]},
    c
  ]
];
antiNullSpaceBasis[c_] := Module[{m},
  m = canonicalM[noncanonicalAntiNullSpaceBasis[c]];

  If[
    m == {{}},
    {Table[0, getD[c]]},
    m
  ]
];


(* MEET AND JOIN *)

getM[t_] := If[isCo[t] == True, getA[t], noncanonicalAntiNullSpaceBasis[getA[t]]];
getC[t_] := If[isContra[t] == True, getA[t], noncanonicalNullSpaceBasis[getA[t]]];