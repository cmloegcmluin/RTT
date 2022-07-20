debug = False;

(*
  
  TEMPERAMENT UTILITIES
  
  
  getD[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the dimensionality.
  
  Examples:
  
  In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
        getD[meantoneM]
    
  Out   3
  
  In    meantoneC = "[4 -4 1⟩";
        getD[meantoneC]
    
  Out   3
  
*)
getD[unparsedT_] := getDPrivate[parseTemperamentData[unparsedT]];
getDPrivate[t_] := colCount[getA[t]];

(*
  
  getR[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the rank.
  
  Examples:
  
  In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
        getR[meantoneM]
    
  Out   2
  
  In    meantoneC = "[4 -4 1⟩";
        getR[meantoneC]
    
  Out   2
  
*)
getR[unparsedT_] := getRPrivate[parseTemperamentData[unparsedT]];
getRPrivate[t_] := If[
  isCo[t],
  MatrixRank[getA[t]],
  getDPrivate[t] - MatrixRank[getA[t]]
];

(*
  
  getN[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the nullity.
  
  Examples:
  
  In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
        getN[meantoneM]
    
  Out   1
  
  In    meantoneC = "[4 -4 1⟩";
        getN[meantoneC]
    
  Out   1
  
*)
getN[unparsedT_] := getNPrivate[parseTemperamentData[unparsedT]];
getNPrivate[t_] := If[
  isContra[t],
  MatrixRank[getA[t]],
  getDPrivate[t] - MatrixRank[getA[t]]
];

(*
  
  CANONICALIZATION
  
  
  canonicalForm[t]
  
  Returns the given temperament representation (mapping or comma basis)
  in canonical form (defactored, then put into Hermite Normal Form).
  
  Examples:
  
  In    someMeantoneM = "[⟨5 8 12] ⟨7 11 16]⟩";
        canonicalForm[someMeantoneM]
  
  Out   "[⟨1 0 -4] ⟨0 1 4]⟩"
  
  In    someMeantoneC = "[-8 8 -2⟩";
        canonicalForm[someMeantoneC]
  
  Out   "[4 -4 1⟩"
  
*)
canonicalForm[unparsedT_] := formatOutput[canonicalFormPrivate[parseTemperamentData[unparsedT]]];
canonicalFormPrivate[t_] := Module[{intervalBasis, canonicalT},
  canonicalT = If[
    isContra[t],
    {canonicalCa[getA[t]], getVariance[t]},
    {canonicalMa[getA[t]], getVariance[t]}
  ];
  intervalBasis = getIntervalBasis[t];
  
  If[
    isStandardPrimeLimitIntervalBasis[intervalBasis],
    canonicalT,
    Join[canonicalT, {intervalBasis}]
  ]
];


(*
  
  DUAL
  
  
  dual[t]
  
  Returns its dual for the given temperament representation
  (if given a mapping, the comma basis, or vice-versa).
  
  Examples:
  
  In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
        dual[meantoneM]
  
  Out   "[4 -4 1⟩"
  
*)
dual[unparsedT_] := formatOutput[dualPrivate[parseTemperamentData[unparsedT]]];
dualPrivate[t_] := If[
  isStandardPrimeLimitIntervalBasis[getIntervalBasis[t]],
  If[
    isContra[t],
    {antiNullSpaceBasis[getA[t]], "co"},
    {nullSpaceBasis[getA[t]], "contra"}
  ],
  nonstandardIntervalBasisDual[t]
];


(*
  
  MERGE
  
  
  mapMerge[t1, t2, t3...]
  
  Merges the given temperaments' maps:
  concatenates their mappings
  and puts the result into canonical form.
  
  Can accept any number of temperaments representations,
  as any combination of mappings or comma bases,
  but returns the temperament as a mapping.
  
  Examples:
  
  In    et5M = "⟨5 8 12]";
        et7M = "⟨7 11 16]";
        mapMerge[et5M, et7M]
  
  Out   "[⟨1 0 -4] ⟨0 1 4]⟩"
  
  In    et7dM = "⟨7 11 16 19]";
        et12M = "⟨12 19 28 34]";
        et22M = "⟨22 35 51 62]";
        mapMerge[et7dM, et12M, et22M]
  
  Out   "[⟨1 0 0 -5] ⟨0 1 0 2] ⟨0 0 1 2]⟩"
  
*)
mapMerge[unparsedT_] := formatOutput[mapMergePrivate[parseTemperamentData[unparsedT]]];
mapMergePrivate[tl___] := Module[{ml, intervalBasisList, intersectedIntervalBasis, tlWithIntersectedIntervalBasis},
  ml = Map[If[isContra[#], dualPrivate[#], #]&, {tl}];
  intervalBasisList = Map[getIntervalBasis, {tl}];
  intersectedIntervalBasis = Apply[intervalBasisIntersection, intervalBasisList];
  tlWithIntersectedIntervalBasis = Map[changeIntervalBasisForM[#, intersectedIntervalBasis]&, ml];
  
  canonicalFormPrivate[{Apply[Join, Map[getA, Map[getM, tlWithIntersectedIntervalBasis]]], "co", intersectedIntervalBasis}]
];

(*
  
  commaMerge[t1, t2, t3...]
  
  Merges the given temperaments' comma bases:
  concatenates their comma bases
  and puts the result into canonical form.
  
  Can accept any number of temperament representations,
  as any combination of mappings or comma bases,
  but returns the temperament as a comma basis.
  
  In    meantoneC = "[4 -4 1⟩";
        porcupineC = "[1 -5 3⟩";
        commaMerge[meantoneC, porcupineC]
  
  Out   "⟨[-11 7 0⟩ [-7 3 1⟩]"
  
  In    mintC = "[2 2 -1 -1⟩";
        meantoneC = "[4 -4 1 0⟩";
        negriC = "[-14 3 4 0⟩";
        commaMerge[mintC, meantoneC, negriC]
  
  Out   "⟨[30 19 0 0⟩ [-26 15 1 0⟩ [-6 2 0 1⟩]"
  
*)
commaMerge[unparsedT_] := formatOutput[commaMergePrivate[parseTemperamentData[unparsedT]]];
commaMergePrivate[tl___] := Module[{cl, intervalBasisList, mergedIntervalBasis, tlWithMergedIntervalBasis},
  cl = Map[If[isContra[#], #, dualPrivate[#]]&, {tl}];
  intervalBasisList = Map[getIntervalBasis, {tl}];
  mergedIntervalBasis = Apply[intervalBasisMerge, intervalBasisList];
  tlWithMergedIntervalBasis = Map[changeIntervalBasisForC[#, mergedIntervalBasis]&, cl];
  
  canonicalFormPrivate[{Apply[Join, Map[getA, Map[getC, tlWithMergedIntervalBasis]]], "contra", mergedIntervalBasis}]
];


(*
  
  INTERVAL BASIS
  
  
  changeIntervalBasis[t, targetIntervalBasis]
  
  Changes the interval basis for the given temperament.
  
  If the target interval basis is not possible
  (such as a superspace for a mapping, or a subspace for
  a comma basis), the function will error.
  
  
  In    meantoneC = "[4 -4 1⟩";
        targetIntervalBasis = "2.3.5.7";
        changeIntervalBasis[meantoneC, targetIntervalBasis]
  
  Out   "[4 -4 1 0⟩"
  
  In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
        targetIntervalBasis = "2.3";
        changeIntervalBasis[meantoneM, targetIntervalBasis]
  
  Out   "[⟨1 0] ⟨0 1]⟩"
  
*)
changeIntervalBasis[unparsedT_] := formatOutput[changeIntervalBasisPrivate[parseTemperamentData[unparsedT]]];
changeIntervalBasisPrivate[t_, targetIntervalBasis_] := If[
  isContra[t],
  changeIntervalBasisForC[t, targetIntervalBasis],
  changeIntervalBasisForM[t, targetIntervalBasis]
];


(*
  
  ADDITION
  
  
  sum[t1, t2]
  
  Sums the given temperaments: if they have the same dimensions
  (same dimensionality, rank (and nullity)),
  and are addable (can be put into a form where
  they are identical except for a single basis vector (or covector, if covariant)),
  entry-wise sums this pair of linearly independent basis (co)vectors,
  recombines them with identical vectors (their linear-dependence basis),
  corrects for negativity, then canonicalizes the result,
  returning a single new temperament with the same dimensions as the inputs.
  
  If the given temperaments are not the same dimensions and addable,
  it will error.
  
  Can accept temperament representations of different variances,
  but it will return a temperament with the same variance
  as the first given temperament representation.
  
  In    meantoneC = "[4 -4 1⟩";
        porcupineC = "[1 -5 3⟩";
        sum[meantoneC, porcupineC]
  
  Out   "[5 -9 4⟩"
  
  In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
        porcupineM = "[⟨1 2 3] ⟨0 3 5]⟩";
        sum[meantoneM, porcupineM]
  
  Out   "[⟨1 1 1] ⟨0 4 9]⟩"
  
*)
sum[unparsedT_] := formatOutput[sumPrivate[parseTemperamentData[unparsedT]]];
sumPrivate[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalFormPrivate[t1input];
  t2 = If[variancesMatch[t1input, t2input], canonicalFormPrivate[t2input], dualPrivate[t2input]];
  
  If[
    t1 == t2,
    t1,
    addition[t1, t2, True]
  ]
];

(*
  
  diff[t1, t2]
  
  Diffs the given temperaments: if they have the same dimensions
  (same dimensionality, rank (and nullity)),
  and are addable (can be put into a form where
  they are identical except for a single basis vector (or basis covector, if covariant)),
  entry-wise diffs this pair of linearly independent basis (co)vectors,
  recombines them with identical vectors (their linear-dependence basis),
  corrects for negativity, then canonicalizes the result,
  returning a single new temperament with the same dimensions as the inputs.
  
  If the given temperaments are not the same dimensions and addable,
  it will error.
  
  Can accept temperament representations of different variances,
  but it will return a temperament with the same variance
  as the first given temperament representation.
  
  In    meantoneC = "[4 -4 1⟩";
        porcupineC = "[1 -5 3⟩";
        diff[meantoneC, porcupineC]
  
  Out   "[-3 -1 2⟩"
  
  In    meantoneM = "[⟨1 0 -4] ⟨0 1 4]⟩";
        porcupineM = "[⟨1 2 3] ⟨0 3 5]⟩";
        diff[meantoneM, porcupineM]
  
  Out   "[⟨1 1 2] ⟨0 2 1]⟩"
  
*)
diff[unparsedT_] := formatOutput[diffPrivate[parseTemperamentData[unparsedT]]];
diffPrivate[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalFormPrivate[t1input];
  t2 = If[variancesMatch[t1input, t2input], canonicalFormPrivate[t2input], dualPrivate[t2input]];
  
  If[
    t1 == t2,
    Error,
    addition[t1, t2, False]
  ]
];



(*
  
  GENERATORS PREIMAGE TRANSVERSAL
  
  
  getGeneratorsPreimageTransversal[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns a generators preimage transversal
  (for each generator, one JI interval that maps to it).
  
  Examples:
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩"
        getGeneratorsPreimageTransversal[meantoneM]
  
  Out   "⟨[1 0 0⟩ [-1 1 0⟩]"
  
*)
getGeneratorsPreimageTransversal[unparsedT_] := formatOutput[getGeneratorsPreimageTransversalPrivate[parseTemperamentData[unparsedT]]];
getGeneratorsPreimageTransversalPrivate[t_] := Module[{ma, decomp, left, snf, right, generatorsPreimageTransversal},
  ma = getA[getM[t]];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  generatorsPreimageTransversal = right.Transpose[snf].left;
  
  {Transpose[generatorsPreimageTransversal], "contra"}
];




(* ___ PRIVATE ___ *)



(* PARSING *)

parseTemperamentData[temperamentData_] := Module[
  {ebk, intervalBasis, variance, ebkVectors},
  
  (*Print["temperamentData: ", temperamentData];*)
  
  If[
    StringMatchQ[ToString[temperamentData], RegularExpression[".*[\\[\\]⟨⟩<>]+.*"]],
    
    ebk = supportMathInEntries[temperamentData];
    
    If[
      StringMatchQ[ebk, RegularExpression["^[\\d\\.\\/]+\\s.*"]],
      
      intervalBasis = First[StringCases[ebk, RegularExpression["^([\\d\\.\\/]+)\\s.*"] -> "$1"]];
      ebk = First[StringCases[ebk, RegularExpression["^[\\d\\.\\/]+\\s(.*)"] -> "$1"]],
      
      intervalBasis = Null;
      ebk = ebk;
    ];
    
    variance = If[isCovariantEBK[ebk], "co", "contra"];
    
    ebkVectors = If[
      variance == "co",
      StringCases[ebk, RegularExpression["[⟨<]([\\d\\-\\+\\*\\/\\.\\,\\s]*)[\\]\\|]\\s*"] -> "$1"],
      StringCases[ebk, RegularExpression["[\\[\\|]([\\d\\-\\+\\*\\/\\.\\,\\s]*)[⟩>]\\s*"] -> "$1"]
    ];
    
    If[
      ToString[intervalBasis] == "Null",
      {Map[parseEBKVector, ebkVectors], variance},
      {Map[parseEBKVector, ebkVectors], variance, parseIntervalBasis[intervalBasis]}
    ],
    
    temperamentData
  ]
];

supportMathInEntries[ebk_] := StringReplace[
  StringReplace[
    StringReplace[
      StringReplace[
        ebk,
        RegularExpression["\\s*\\*\\s*"] -> "*"
      ],
      RegularExpression["\\s*\\/\\s*"] -> "/"
    ],
    RegularExpression["\\s*\\+\\s+"] -> "+"
  ],
  RegularExpression["\\s*\\-\\s+"] -> "-"
];

isCovariantEBK[ebk_] := StringMatchQ[ebk, RegularExpression["^[\\[]?\\s*[<⟨][^\\[]*"]];
parseEBKVector[ebkVector_] := Map[ToExpression, StringSplit[ebkVector, RegularExpression["(?:\\s*\\,\\s*)|\\s+"]]];

parseIntervalBasis[intervalBasisString_] := Map[ToExpression, StringSplit[intervalBasisString, "."]];

toEBK[t_] := If[
  ListQ[t],
  (*Print["wow why am i still doing this ", t];*)
  If[
    isContra[t],
    If[
      getN[t] == 1,
      vectorToEBK[First[getA[t]]],
      ToString[StringForm["⟨``]", StringRiffle[ Map[vectorToEBK, getA[t]]]]]
    ],
    If[
      getR[t] == 1,
      covectorToEBK[First[getA[t]]],
      ToString[StringForm["[``⟩", StringRiffle[ Map[covectorToEBK, getA[t]]]]]
    ]
  ],
  (*Print["should be fine! ", t];*)
  t
];

outputPrecision = 4;
vectorToEBK[vector_] := ToString[StringForm["[``⟩", StringRiffle[Map[formatNumber, vector]]]];
covectorToEBK[covector_] := ToString[StringForm["⟨``]", StringRiffle[Map[formatNumber, covector]]]];

formatNumber[entry_] := ToString[If[IntegerQ[entry], entry, SetAccuracy[N[entry], outputPrecision]]];
formatNumberList[l_] := Map[formatNumber, l];

toDisplay[t_] := If[ListQ[t], MatrixForm[Map[formatNumberList, If[isContra[t], Transpose[getA[t]], getA[t]]]], t];

formatOutput[output_] := If[
  format == "EBK",
  toEBK[output],
  If[
    format == "display",
    toDisplay[output],
    output
  ]
];

printWrapper[string___] := Apply[Print, {string}];

parseQuotientSet[inputQuotientSetString_, t_] := Module[
  {quotientSetString, quotients},
  
  quotientSetString = If[
    StringMatchQ[inputQuotientSetString, RegularExpression["^\\{.*\\}$"]],
    inputQuotientSetString,
    "{" <> inputQuotientSetString <> "}"
  ];
  
  quotients = Map[ToExpression, StringCases[quotientSetString, RegularExpression["([\\d\\/]+)[\\,\\s\\}]+"] -> "$1"]];
  
  {
    padVectorsWithZerosUpToD[
      Map[quotientToPcv, quotients],
      getDPrivate[t]
    ],
    "contra"
  }
];

(* format = "EBK"; *)
format = "display";
(* format = "Wolfram"; *)


(* LIST UTILITIES *)

getGcd[l_] := Apply[GCD, l];
divideOutGcd[l_] := Module[{gcd}, gcd = getGcd[l]; If[gcd == 0, l, l / gcd]];
multByLcd[l_] := Apply[LCM, Denominator[l]] * l;

leadingEntry[l_] := First[Select[l, # != 0&, 1]];
trailingEntry[l_] := leadingEntry[Reverse[l]];

allZerosL[l_] := AllTrue[l, # == 0&];


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

getA[t_] := Part[t, 1];
getVariance[t_] := Part[t, 2];

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
}, getVariance[t]];
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
}, getVariance[t]];
multiply[t_, variance_] := Module[
  {a},
  
  a = Apply[Dot, Map[If[isContra[#], Transpose[getA[#]], getA[#]]&, t]];
  
  If[
    isCo[{{}, variance}],
    {a, variance},
    {Transpose[a], variance}
  ]
];
inverse[t_] := {Inverse[getA[t]], getVariance[t]};
transpose[t_] := {getA[t], If[getVariance[t] == "co", "contra", "co"]};


(* CANONICALIZATION *)

hnf[a_] := Last[HermiteDecomposition[a]];

hermiteRightUnimodular[a_] := Transpose[First[HermiteDecomposition[Transpose[a]]]];
colHermiteDefactor[a_] := Take[Inverse[hermiteRightUnimodular[a]], MatrixRank[a]];

canonicalMa[ma_] := If[
  allZeros[ma],
  {Table[0, colCount[ma]]},
  removeUnneededZeroRows[hnf[colHermiteDefactor[ma]]]
];
canonicalCa[ca_] := antiTranspose[canonicalMa[antiTranspose[ca]]];


(* DUAL *)

noncanonicalNullSpaceBasis[ma_] := reverseEachCol[NullSpace[ma]];
noncanonicalAntiNullSpaceBasis[ca_] := NullSpace[ca];

nullSpaceBasis[ma_] := Module[{ca},
  ca = canonicalCa[noncanonicalNullSpaceBasis[ma]];
  
  If[
    ca == {{}},
    {Table[0, getDPrivate[ma]]},
    ca
  ]
];
antiNullSpaceBasis[ca_] := Module[{ma},
  ma = canonicalMa[noncanonicalAntiNullSpaceBasis[ca]];
  
  If[
    ma == {{}},
    {Table[0, getDPrivate[ca]]},
    ma
  ]
];

nonstandardIntervalBasisDual[t_] := If[
  isContra[t],
  {antiNullSpaceBasis[getA[t]], "co", getIntervalBasis[t]},
  {nullSpaceBasis[getA[t]], "contra", getIntervalBasis[t]}
];


(* MERGE *)

getM[t_] := If[isCo[t] == True, t, dualPrivate[t]];
getC[t_] := If[isContra[t] == True, t, dualPrivate[t]];


(* INTERVAL BASIS *)

intervalBasisMerge[intervalBasisList___] := Module[{concattedIntervalBasis, concattedFormalPrimesA},
  concattedIntervalBasis = Apply[Join, {intervalBasisList}];
  concattedFormalPrimesA = padVectorsWithZerosUpToD[Map[quotientToPcv, concattedIntervalBasis], getIntervalBasisDimension[concattedIntervalBasis]];
  
  canonicalIntervalBasis[Map[pcvToQuotient, concattedFormalPrimesA]]
];

intervalBasisIntersectionBinary[intervalBasis1_, intervalBasis2_] := Module[{intervalBasisDimension, formalPrimesA1, formalPrimesA2, allZerosFillerFormalPrimesA, blockA, intersectedFormalPrimesA, blockLHalf1, blockLHalf2},
  intervalBasisDimension = Max[getIntervalBasisDimension[intervalBasis1], getIntervalBasisDimension[intervalBasis2]];
  formalPrimesA1 = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis1], intervalBasisDimension];
  formalPrimesA2 = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis2], intervalBasisDimension];
  
  allZerosFillerFormalPrimesA = Table[Table[0, Length[First[formalPrimesA2]]], Length[formalPrimesA2]];
  
  blockA = hnf[ArrayFlatten[
    {
      {formalPrimesA1, formalPrimesA1},
      {formalPrimesA2, allZerosFillerFormalPrimesA}
    }
  ]];
  
  intersectedFormalPrimesA = {};
  Do[
    blockLHalf1 = Take[blockL, Length[blockL] / 2];
    blockLHalf2 = Take[blockL, {Length[blockL] / 2 + 1, Length[blockL]}];
    If[allZerosL[blockLHalf1], intersectedFormalPrimesA = Join[intersectedFormalPrimesA, {blockLHalf2}]],
    {blockL, blockA}
  ];
  intersectedFormalPrimesA = If[Length[intersectedFormalPrimesA] == 0, {0}, intersectedFormalPrimesA];
  
  canonicalIntervalBasis[Map[pcvToQuotient, intersectedFormalPrimesA]]
];

intervalBasisIntersection[intervalBasisList___] := Module[{intersectedIntervalBasis},
  intersectedIntervalBasis = First[{intervalBasisList}];
  
  Do[
    intersectedIntervalBasis = intervalBasisIntersectionBinary[intersectedIntervalBasis, intervalBasis],
    {intervalBasis, Drop[{intervalBasisList}, 1]}
  ];
  
  canonicalIntervalBasis[intersectedIntervalBasis]
];

isSubspaceOf[candidateSubspaceIntervalBasis_, candidateSuperspaceIntervalBasis_] :=
    intervalBasisMerge[candidateSubspaceIntervalBasis, candidateSuperspaceIntervalBasis] == candidateSuperspaceIntervalBasis;

canonicalIntervalBasis[intervalBasis_] := Module[{formalPrimesA, canonicalFormalPrimesA},
  formalPrimesA = padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis], getIntervalBasisDimension[intervalBasis]];
  canonicalFormalPrimesA = antiTranspose[removeAllZeroRows[hnf[antiTranspose[formalPrimesA]]]];
  
  If[
    Length[canonicalFormalPrimesA] == 0,
    {1},
    Map[super, Map[pcvToQuotient, canonicalFormalPrimesA]]
  ]
];

changeIntervalBasisForM[m_, targetSubspaceIntervalBasis_] := If[
  getIntervalBasis[m] == targetSubspaceIntervalBasis,
  m,
  If[
    isSubspaceOf[getIntervalBasis[m], targetSubspaceIntervalBasis],
    Error,
    canonicalFormPrivate[{getA[m].Transpose[getIntervalRebaseForM[getIntervalBasis[m], targetSubspaceIntervalBasis]], "co", targetSubspaceIntervalBasis}]
  ]
];

changeIntervalBasisForC[c_, targetSuperspaceIntervalBasis_] := If[
  getIntervalBasis[c] == targetSuperspaceIntervalBasis,
  c,
  If[
    isSubspaceOf[getIntervalBasis[c], targetSuperspaceIntervalBasis],
    canonicalFormPrivate[{Transpose[Transpose[getIntervalRebaseForC[getIntervalBasis[c], targetSuperspaceIntervalBasis]].Transpose[getA[c]]], "contra", targetSuperspaceIntervalBasis}],
    Error
  ]
];

(* express the target formal primes in terms of the initial formal primes*)
getIntervalRebaseForM[originalSuperspaceIntervalBasis_, targetSubspaceIntervalBasis_] := Module[
  {
    intervalBasisDimension,
    targetSubspaceFormalPrimesA,
    originalSuperspaceFormalPrimesA,
    intervalRebase,
    intervalRebaseCol,
    intervalRebaseColEntry,
    remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry
  },
  
  intervalBasisDimension = getIntervalBasisDimension[Join[originalSuperspaceIntervalBasis, targetSubspaceIntervalBasis]];
  targetSubspaceFormalPrimesA = padVectorsWithZerosUpToD[Map[quotientToPcv, targetSubspaceIntervalBasis], intervalBasisDimension];
  originalSuperspaceFormalPrimesA = padVectorsWithZerosUpToD[Map[quotientToPcv, originalSuperspaceIntervalBasis], intervalBasisDimension];
  
  intervalRebase = {};
  
  Do[
    intervalRebaseCol = {};
    remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry = targetSubspaceFormalPrimesAEntry;
    Do[
      intervalRebaseColEntry = 0;
      
      While[
        isNumeratorFactor[remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry, originalSuperspaceFormalPrimesAEntry],
        intervalRebaseColEntry += 1;
        remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry -= originalSuperspaceFormalPrimesAEntry
      ];
      
      While[
        isDenominatorFactor[remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry, originalSuperspaceFormalPrimesAEntry],
        intervalRebaseColEntry -= 1;
        remainingToBeFactorizedTargetSubspaceFormalPrimesAEntry += originalSuperspaceFormalPrimesAEntry
      ];
      
      intervalRebaseCol = Join[intervalRebaseCol, {intervalRebaseColEntry}],
      {originalSuperspaceFormalPrimesAEntry, originalSuperspaceFormalPrimesA}
    ];
    intervalRebase = Join[intervalRebase, {intervalRebaseCol}],
    {targetSubspaceFormalPrimesAEntry, targetSubspaceFormalPrimesA}
  ];
  
  intervalRebase
];

(* yes, just swapping initial and target, that's all! *)
getIntervalRebaseForC[originalSubspaceIntervalBasis_, targetSuperspaceIntervalBasis_] := getIntervalRebaseForM[targetSuperspaceIntervalBasis, originalSubspaceIntervalBasis];

getPrimes[count_] := Map[Prime, Range[count]];

quotientToPcv[quotient_] := Module[{factorization, greatestPrime, count, primes, pcv, currentPrimeIndex},
  factorization = FactorInteger[quotient];
  greatestPrime = First[Last[factorization]];
  count = PrimePi[greatestPrime];
  primes = getPrimes[count];
  pcv = Table[0, count];
  currentPrimeIndex = 1;
  
  If[Length[primes] == 0,
    {0},
    Do[
      While[
        primes[[currentPrimeIndex]] < First[factorizationEntry],
        currentPrimeIndex += 1
      ];
      pcv[[currentPrimeIndex]] = Last[factorizationEntry],
      {factorizationEntry, factorization}
    ];
    pcv
  ]
];

pcvToQuotient[pcv_] := Module[{quotient, primeIndex},
  quotient = 1;
  primeIndex = 1;
  Do[
    quotient = quotient * Prime[primeIndex]^iEntry;
    primeIndex += 1,
    {iEntry, pcv}
  ];
  
  quotient
];

getIntervalBasisDimension[intervalBasis_] := Max[1, PrimePi[Max[Map[First, Map[Last, Map[FactorInteger, intervalBasis]]]]]];

padVectorsWithZerosUpToD[a_, d_] := Map[PadRight[#, d]&, a];

super[quotient_] := If[quotient < 1, Denominator[quotient] / Numerator[quotient], quotient];

getStandardPrimeLimitIntervalBasis[t_] := getPrimes[getDPrivate[t]];

isStandardPrimeLimitIntervalBasis[intervalBasis_] := canonicalIntervalBasis[intervalBasis] == getPrimes[Length[intervalBasis]];

getIntervalBasis[t_] := If[
  Length[t] == 3,
  Part[t, 3],
  getStandardPrimeLimitIntervalBasis[t]
];

signsMatch[integer1_, integer2_] := Sign[integer1] == 0 || Sign[integer2] == 0 || Sign[integer1] == Sign[integer2];

factorizationIsAcceptableForThisPrimesCounts[integer1_, integer2_] := Abs[integer1] >= Abs[integer2] && signsMatch[integer1, integer2];

isNumeratorFactor[subspaceFEntry_, superspaceFEntry_] := !MemberQ[MapThread[
  factorizationIsAcceptableForThisPrimesCounts,
  {subspaceFEntry, subspaceFEntry - superspaceFEntry}
], False];
isDenominatorFactor[subspaceFEntry_, superspaceFEntry_] := !MemberQ[MapThread[
  factorizationIsAcceptableForThisPrimesCounts,
  {subspaceFEntry, subspaceFEntry + superspaceFEntry}
], False];

getFormalPrimesA[t_] := Module[{intervalBasis},
  intervalBasis = getIntervalBasis[t];
  padVectorsWithZerosUpToD[Map[quotientToPcv, intervalBasis], getIntervalBasisDimension[intervalBasis]]
];


(* ADDITION *)

addition[t1_, t2_, isSum_] := If[
  dimensionsDoNotMatch[t1, t2] || intervalBasesDoNotMatch[t1, t2],
  Error,
  Module[{linearDependenceBasis},
    linearDependenceBasis = getLinearDependenceBasis[t1, t2];
    
    If[
      linearDependenceBasis === Error, (* not addable *)
      Error,
      addableAddition[t1, t2, linearDependenceBasis, isSum]
    ]
  ]
];

addableAddition[t1_, t2_, linearDependenceBasis_, isSum_] := Module[
  {
    t1LinearIndependenceBasisVector,
    t2LinearIndependenceBasisVector,
    t1t2LinearIndependenceBasisVector
  },
  
  t1LinearIndependenceBasisVector = getLinearIndependenceBasisVector[t1, linearDependenceBasis];
  t2LinearIndependenceBasisVector = getLinearIndependenceBasisVector[t2, linearDependenceBasis];
  
  t1t2LinearIndependenceBasisVector = If[
    isSum,
    t1LinearIndependenceBasisVector + t2LinearIndependenceBasisVector,
    t1LinearIndependenceBasisVector - t2LinearIndependenceBasisVector
  ];
  
  canonicalFormPrivate[{Join[linearDependenceBasis, {t1t2LinearIndependenceBasisVector}], getVariance[t1]}]
];

getLinearIndependenceBasisVector[t_, linearDependenceBasis_] := Module[{a, linearIndependenceBasisVector},
  a = addabilizationDefactor[t, linearDependenceBasis];
  linearIndependenceBasisVector = Last[a];
  If[isNegative[a, isContra[t]], linearIndependenceBasisVector = -linearIndependenceBasisVector];
  
  linearIndependenceBasisVector
];

addabilizationDefactor[t_, linearDependenceBasis_] := Module[
  {
    grade,
    explicitLinearDependenceBasisFormOfA
  },
  
  grade = getGrade[t];
  explicitLinearDependenceBasisFormOfA = getInitialExplicitLinearDependenceBasisFormOfA[t, linearDependenceBasis, grade];
  
  If[
    isLinearlyDependent[linearDependenceBasis],
    addabilizationDefactorWithNonemptyLinearDependenceBasis[t, linearDependenceBasis, grade, explicitLinearDependenceBasisFormOfA],
    explicitLinearDependenceBasisFormOfA
  ]
];

addabilizationDefactorWithNonemptyLinearDependenceBasis[t_, linearDependenceBasis_, grade_, explicitLdbFormOfAInput_] := Module[
  {
    explicitLinearDependenceBasisFormOfA,
    d,
    linearDependence,
    enfactoring,
    multiples,
    equations,
    answer,
    result
  },
  
  explicitLinearDependenceBasisFormOfA = explicitLdbFormOfAInput;
  d = getDPrivate[t];
  linearDependence = getLinearDependence[linearDependenceBasis];
  enfactoring = getGreatestFactor[explicitLinearDependenceBasisFormOfA];
  
  multiples = Table[Subscript[x, index], {index, linearDependence}];
  equations = Map[
    Function[
      dIndex,
      Mod[explicitLinearDependenceBasisFormOfA[[grade]][[dIndex]] + Total[Map[
        Function[multiplesIndex, multiples[[multiplesIndex]] * linearDependenceBasis[[multiplesIndex]][[dIndex]]],
        Range[linearDependence]
      ]], enfactoring] == 0
    ],
    Range[d]
  ];
  answer = FindInstance[equations, multiples, Integers];
  result = Values[Association[answer]];
  explicitLinearDependenceBasisFormOfA[[grade]] = divideOutGcd[explicitLinearDependenceBasisFormOfA[[grade]] + getLinearDependenceBasisLinearCombination[linearDependenceBasis, result]];
  
  explicitLinearDependenceBasisFormOfA
];

variancesMatch[t1_, t2_] := getVariance[t1] == getVariance[t2];

getLinearDependenceBasis[t1_, t2_] := Module[{linearDependenceBasis},
  linearDependenceBasis = removeAllZeroRows[getA[dualPrivate[
    If[
      isContra[t1],
      mapMergePrivate[t1, t2],
      commaMergePrivate[t1, t2]
    ]
  ]]];
  
  If[
    isAddable[linearDependenceBasis, t1],
    linearDependenceBasis,
    Error
  ]
];

isAddable[linearDependenceBasis_, t_] := getLinearDependence[linearDependenceBasis] === getGrade[t] - 1;

getLinearDependence[linearDependenceBasis_] := Length[linearDependenceBasis];

dimensionsDoNotMatch[t1_, t2_] := getRPrivate[t1] != getRPrivate[t2] || getDPrivate[t1] != getDPrivate[t2];

intervalBasesDoNotMatch[t1_, t2_] := getIntervalBasis[t1] != getIntervalBasis[t2];

getGrade[t_] := If[isContra[t], getNPrivate[t], getRPrivate[t]];

isLinearlyDependent[linearDependenceBasis_] := getLinearDependence[linearDependenceBasis] > 0;

getInitialExplicitLinearDependenceBasisFormOfA[t_, linearDependenceBasis_, grade_] := Module[
  {
    linearIndependenceBasisSource,
    explicitLinearDependenceBasisFormOfA
  },
  
  linearIndependenceBasisSource = getA[If[isContra[t], getC[t], getM[t]]];
  explicitLinearDependenceBasisFormOfA = linearDependenceBasis;
  
  Do[
    candidate = hnf[Join[linearDependenceBasis, {candidateLinearIndependenceBasisVector}]];
    If[
      Length[explicitLinearDependenceBasisFormOfA] < grade && MatrixRank[candidate] > Length[linearDependenceBasis],
      explicitLinearDependenceBasisFormOfA = Join[explicitLinearDependenceBasisFormOfA, {candidateLinearIndependenceBasisVector}]
    ],
    {candidateLinearIndependenceBasisVector, linearIndependenceBasisSource}
  ];
  Take[explicitLinearDependenceBasisFormOfA, grade]
];

getGreatestFactor[a_] := Det[getGreatestFactorA[a]];

getGreatestFactorA[a_] := Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]];

getLinearDependenceBasisLinearCombination[linearDependenceBasis_, linearDependenceBasisMultiplePermutation_] := Total[MapThread[
  #1 * #2&,
  {linearDependenceBasis, linearDependenceBasisMultiplePermutation}
]];

getLargestMinorsL[a_] := divideOutGcd[First[Minors[a, MatrixRank[a]]]];

isNegative[a_, contra_] := Module[{largestMinorsL, entryFn, normalizingEntry},
  largestMinorsL = getLargestMinorsL[a];
  entryFn = If[contra, trailingEntry, leadingEntry];
  normalizingEntry = entryFn[largestMinorsL];
  
  normalizingEntry < 0
];
