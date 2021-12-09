f = 0;
p = 0;
test[fn_, arg_, expectation_] := Module[{actual},
  actual = fn[arg];

  If[
    actual === expectation,
    p += 1,
    f += 1;
    Print[fn, "[", arg, "] != ", expectation, "; actual result was: ", actual]
  ]
];
test2args[fn_, arg1_, arg2_, expectation_] := Module[{actual},
  actual = fn[arg1, arg2];

  If[
    actual === expectation,
    p += 1,
    f += 1;
    Print[fn, "[", arg1, ",", arg2, "] != ", expectation, "; actual result was: ", actual]
  ]
];
test3args[fn_, arg1_, arg2_, arg3_, expectation_] := Module[{actual},
  actual = fn[arg1, arg2, arg3];

  If[
    actual === expectation,
    p += 1,
    f += 1;
    Print[fn, "[", arg1, ",", arg2, "," arg3, "] != ", expectation, "; actual result was: ", actual]
  ]
];


(* TEMPERAMENT UTILITIES *)

(* getD *)
test[getD, {{{0}}, "co"}, 1];
test[getD, {{{0}}, "contra"}, 1];
test[getD, {{{0, 0}}, "co"} , 2];
test[getD, {{{0, 0}}, "contra"} , 2];
test[getD, {{{0}, {0}}, "co"}, 1];
test[getD, {{{0}, {0}}, "contra"}, 1];
test[getD, {IdentityMatrix[2], "co"}, 2];
test[getD, {IdentityMatrix[2], "contra"}, 2];
test[getD, {{{1, 0, -4}, {0, 1, 4}}, "co"}, 3];
test[getD, {{{4, -4, 1}}, "contra"}, 3];
test[getD, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "co"}, 4];
test[getD, {{{4, -4, 1, 0}}, "contra"}, 4];

(* getR *)
test[getR, {{{0}}, "co"}, 0];
test[getR, {{{0}}, "contra"}, 1];
test[getR, {{{0, 0}}, "co"} , 0];
test[getR, {{{0, 0}}, "contra"} , 2];
test[getR, {{{0}, {0}}, "co"}, 0];
test[getR, {{{0}, {0}}, "contra"}, 1];
test[getR, {IdentityMatrix[2], "co"}, 2];
test[getR, {IdentityMatrix[2], "contra"}, 0];
test[getR, {{{1, 0, -4}, {0, 1, 4}}, "co"}, 2];
test[getR, {{{4, -4, 1}}, "contra"}, 2];
test[getR, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "co"}, 2];
test[getR, {{{4, -4, 1, 0}}, "contra"}, 3];

(* getN *)
test[getN, {{{0}}, "co"}, 1];
test[getN, {{{0}}, "contra"}, 0];
test[getN, {{{0, 0}}, "co"} , 2];
test[getN, {{{0, 0}}, "contra"} , 0];
test[getN, {{{0}, {0}}, "co"}, 1];
test[getN, {{{0}, {0}}, "contra"}, 0];
test[getN, {IdentityMatrix[2], "co"}, 0];
test[getN, {IdentityMatrix[2], "contra"}, 2];
test[getN, {{{1, 0, -4}, {0, 1, 4}}, "co"}, 1];
test[getN, {{{4, -4, 1}}, "contra"}, 1];
test[getN, {{{1, 0, -4, 0}, {0, 1, 4, 0}}, "co"}, 2];
test[getN, {{{4, -4, 1, 0}}, "contra"}, 1];


(* CANONICALIZATION *)

(* canonicalForm *)
test[canonicalForm, {{{12, 0, 0}, {19, 0, 0}}, "a"}, {{{1, 0, 0}}, "a"}];
test[canonicalForm, {{{1, 1, 0}, {0, 1, 4}}, "a"}, {{{1, 0, -4}, {0, 1, 4}}, "a"}];
test[canonicalForm, {{{12, 19, 28}}, "a"}, {{{12, 19, 28}}, "a"}];
test[canonicalForm, {{{7, 11, 16}, {22, 35, 51}}, "a"}, {{{1, 2, 3}, {0, 3, 5}}, "a"}];
test[canonicalForm, {{{3, 0, -1}, {0, 3, 5}}, "a"}, {{{1, 2, 3}, {0, 3, 5}}, "a"}];
test[canonicalForm, {{{1, 2, 3}, {0, 3, 5}}, "a"}, {{{1, 2, 3}, {0, 3, 5}}, "a"}];
test[canonicalForm, {{{0, 1, 4, 10}, {1, 0, -4, -13}}, "a"}, {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "a"}];
test[canonicalForm, {{{10, 13, 12, 0}, {-1, -1, 0, 3}}, "a"}, {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "a"}];
test[canonicalForm, {{{5, 8, 0}, {0, 0, 1}}, "a"}, {{{5, 8, 0}, {0, 0, 1}}, "a"}];
test[canonicalForm, {{{2, 0, 11, 12}, {0, 1, -2, -2}}, "a"}, {{{2, 0, 11, 12}, {0, 1, -2, -2}}, "a"}];
test[canonicalForm, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "a"}, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "a"}];
test[canonicalForm, {{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "a"}, {{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "a"}];
test[canonicalForm, {{{12, 19, 28}, {26, 43, 60}}, "a"}, {{{1, 8, 0}, {0, 11, -4}}, "a"}];
test[canonicalForm, {{{17, 16, -4}, {4, -4, 1}}, "a"}, {{{1, 0, 0}, {0, 4, -1}}, "a"}];
test[canonicalForm, {{{6, 5, -4}, {4, -4, 1}}, "a"}, {{{2, 1, -1}, {0, 2, -1}}, "a"}];
test[canonicalForm, {{{12, 19, 28}, {0, 0, 0}}, "a"}, {{{12, 19, 28}}, "a"}];
test[canonicalForm, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {1, 1, 0, -3}}, "a"}, {{{1, 0, 0, -5}, {0, 1, 0, 2}}, "a"}];
test[canonicalForm, {{{0, 0}}, "a"}, {{{0, 0}}, "a"}];
test[canonicalForm, {IdentityMatrix[3], "a"}, {IdentityMatrix[3], "a"}];
test[canonicalForm, {{{1, 0, -4}, {0, 1, 4}, {0, 0, 0}}, "a", {{1, 0, -4}, {0, 1, 4}}, "a"}];
test[canonicalForm, {{{12, 19, 28, 0}}, "a"}, {{{12, 19, 28, 0}}, "a"}];
test[canonicalForm, {{{0, 0, 0}, {0, 0, 0}}, "a"}, {{{0, 0, 0}}, "a"}];


(* DUAL *)

(* dual *)
verifyDuals[m_, c_] := Module[{dualM, dualC},
  dualC = dual[m];
  dualM = dual[c];

  If[
    dualC == canonicalForm[c] && dualM == canonicalForm[m],
    p += 1,
    f += 1;
    Print["verifyDuals[", m, ", ", c, "]; dualC: ", dualC, " canonicalForm[c]: ", canonicalForm[c], " dualM: ", dualM, " canonicalForm[m]: ", canonicalForm[m]]
  ];
];

verifyDuals[{{{1, 0, -4}, {0, 1, 4}}, "co"}, {{{4, -4, 1}}, "contra"}];
verifyDuals[{{{1, 0, 0}, {0, -4, 9}}, "co"}, {{{0, 9, 4}}, "contra"}];
verifyDuals[{{{0}}, "co"}, {IdentityMatrix[1], "contra"}];
verifyDuals[{{{0, 0}}, "co"}, {IdentityMatrix[2], "contra"}];
verifyDuals[{{{0, 0, 0}}, "co"}, {IdentityMatrix[3], "contra"}];
verifyDuals[{IdentityMatrix[1], "co"}, {{{0}}, "contra"}];
verifyDuals[{IdentityMatrix[2], "co"}, {{{0, 0}}, "contra"}];
verifyDuals[{IdentityMatrix[3], "co"}, {{{0, 0, 0}}, "contra"}];
verifyDuals[{{{12, 19}}, "co"}, {{{-19, 12}}, "contra"}];


(* MEET AND JOIN *)

(* basic examples *)

et5M5 = {{{5, 8, 12}}, "co"};
et5C5 = {{{-8, 5, 0}, {-4, 1, 1}}, "contra"};
et7M5 = {{{7, 11, 16}}, "co"};
et7C5 = {{{-11, 7, 0}, {-7, 3, 1}}, "contra"};
meantoneM5 = {{{1, 0, -4}, {0, 1, 4}}, "co"};
meantoneC5 = {{{4, -4, 1}}, "contra"};
porcupineM5 = {{{1, 2, 3}, {0, 3, 5}}, "co"};
porcupineC5 = {{{1, -5, 3}}, "contra"};

test[dual, et5C5, et5M5];
test[dual, et7C5, et7M5];
test[dual, meantoneC5, meantoneM5];
test[dual, porcupineC5, porcupineM5];

test2args[join, et5M5, et7M5, meantoneM5];
test2args[meet, meantoneC5, porcupineC5, et7C5];

(* prove out that you can specify temperaments by either their mappings or their comma bases *)

test2args[join, {et5M5, et7C5}, meantoneM5];
test2args[meet, {meantoneM5, porcupineC5}, et7C5];
test2args[join, {et5C5, et7M5}, meantoneM5];
test2args[meet, {meantoneC5, porcupineM5}, et7C5];
test2args[join, {et5C5, et7C5}, meantoneM5];
test2args[meet, {meantoneM5, porcupineM5}, et7C5];

(* prove out that you can meet or join more than 2 temperaments at a time *)

et7dLimit7 = {{{7, 11, 16, 19}}, "co"};
et12Limit7 = {{{12, 19, 28, 34}}, "co"};
et22Limit7 = {{{22, 35, 51, 62}}, "co"};
marvel = {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"};
test3args[join, et7dLimit7, et12Limit7, et22Limit7, marvel];

mintC7 = {{{2, 2, -1, -1}}, "contra"};
meantoneC7 = {{{4, -4, 1, 0}}, "contra"};
negriC7 = {{{-14, 3, 4, 0}}, "contra"};
et19dC7 = dual[{{{19, 30, 44, 54}}, "co"}];
test3args[meet, mintC7, meantoneC7, negriC7, et19dC7];

(* examples from Meet and Join page *)

meantoneComma7 = {-4, 4, -1, 0};
starlingComma7 = {1, 2, -3, 1};
septimalComma7 = {6, -2, 0, -1};
porcupineComma7 = {1, -5, 3, 0};
marvelComma7 = {-5, 2, 2, -1};
gamelisma7 = {-10, 1, 0, 3};
sensamagicComma7 = {0, -5, 1, 2};

meantoneComma11 = {-4, 4, -1, 0, 0};
starlingComma11 = {1, 2, -3, 1, 0};
keenanisma11 = {-7, -1, 1, 1, 1};
marvelComma11 = {-5, 2, 2, -1, 0};
septimalComma11 = {6, -2, 0, -1, 0};
ptolemisma11 = {2, -2, 2, 0, -1};
telepathma11 = {-1, -3, 1, 0, 1};
mothwellsma11 = {-1, 2, 0, -2, 1};
rastma11 = {-1, 5, 0, 0, -2};
sensamagicComma11 = {0, -5, 1, 2, 0};
werckisma11 = {-3, 2, -1, 2, -1};
valinorsma11 = {4, 0, -2, -1, 1};

meantoneM11 = {{{1, 0, -4, -13, -25}, {0, 1, 4, 10, 18}}, "co"};
meantoneC11 = {{meantoneComma11, starlingComma11, mothwellsma11} , "contra"};
meanpopM11 = {{{1, 0, -4, -13, 24}, {0, 1, 4, 10, -13}}, "co"};
meanpopC11 = {{meantoneComma11, starlingComma11, keenanisma11} , "contra"};
marvelM11 = {{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "co"};
marvelC11 = {{marvelComma11, keenanisma11} , "contra"};
porcupineM11 = {{{1, 2, 3, 2, 4}, {0, 3, 5, -6, 4}}, "co"};
porcupineC11 = {{telepathma11, septimalComma11, ptolemisma11} , "contra"};
et31M11 = {{{31, 49, 72, 87, 107}}, "co"};
et31C11 = {{{-49, 31, 0, 0, 0}, {-45, 27, 1, 0, 0}, {-36, 21, 0, 1, 0}, {-24, 13, 0, 0, 1}} , "contra"};
meantoneM7 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
meantoneC7 = {{meantoneComma7, starlingComma7} , "contra"};
porcupineM7 = {{{1, 2, 3, 2}, {0, 3, 5, -6}}, "co"};
porcupineC7 = {{septimalComma7, porcupineComma7} , "contra"};
miracleM11 = {{{1, 1, 3, 3, 2}, {0, 6, -7, -2, 15}}, "co"};
miracleC11 = {{marvelComma11, rastma11, keenanisma11} , "contra"};
magicM11 = {{{1, 0, 2, -1, 6}, {0, 5, 1, 12, -8}}, "co"};
magicC11 = {{marvelComma11, sensamagicComma11, ptolemisma11} , "contra"};
et41M11 = {{{41, 65, 95, 115, 142}}, "co"};
et41C11 = {{{-65, 41, 0, 0, 0}, {-15, 8, 1, 0, 0}, {-25, 14, 0, 1, 0}, {-32, 18, 0, 0, 1}} , "contra"};
miracleM7 = {{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"};
miracleC7 = {{marvelComma7, gamelisma7} , "contra"};
magicM7 = {{{1, 0, 2, -1}, {0, 5, 1, 12}}, "co"};
magicC7 = {{marvelComma7, sensamagicComma7} , "contra"};
et41M7 = {{{41, 65, 95, 115}}, "co"};
et41C7 = {{{-65, 41, 0, 0}, {-15, 8, 1, 0}, {-25, 14, 0, 1}} , "contra"};
mothraM11 = {{{1, 1, 0, 3, 5}, {0, 3, 12, -1, -8}}, "co"};
mothraC11 = {{meantoneComma11, mothwellsma11, keenanisma11} , "contra"};
mothraM7 = {{{1, 1, 0, 3}, {0, 3, 12, -1}}, "co"};
mothraC7 = {{meantoneComma7, gamelisma7} , "contra"};
portentM11 = {{{1, 1, 0, 3, 5}, {0, 3, 0, -1, 4}, {0, 0, 1, 0, -1}}, "co"};
portentC11 = {{keenanisma11, werckisma11} , "contra"};
gamelanM7 = {{{1, 1, 0, 3}, {0, 3, 0, -1}, {0, 0, 1, 0}}, "co"};
gamelanC7 = {{gamelisma7}, "contra"};
marvelM7 = {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"};
marvelC7 = {{marvelComma7}, "contra"};

test[dual, meantoneC11, meantoneM11];
test[dual, meanpopC11, meanpopM11];
test[dual, marvelC11, marvelM11];
test[dual, porcupineC11, porcupineM11];
test[dual, et31C11, et31M11];
test[dual, meantoneC7, meantoneM7];
test[dual, porcupineC7, porcupineM7];
test[dual, miracleC11, miracleM11];
test[dual, magicC11, magicM11];
test[dual, et41C11, et41M11];
test[dual, miracleC7, miracleM7];
test[dual, magicC7, magicM7];
test[dual, et41C7, et41M7];
test[dual, mothraC11, mothraM11];
test[dual, mothraC7, mothraM7];
test[dual, portentC11, portentM11];
test[dual, gamelanC7, gamelanM7];
test[dual, marvelC7, marvelM7];

(*⋎ = MEET, ⋏ = JOIN *)

(*Meantone⋎Meanpop = [<31 49 72 87 107|] = 31, where "31" is the shorthand notation for the 31edo patent val.*)
test2args[meet, meantoneC11, meanpopC11, et31C11];

(*Meantone⋏Meanpop = [<1 0 -4 -13 0|, <0 1 4 10 0|, <0 0 0 0 1|] = <81/80, 126/125>*)
test2args[join, meantoneM11, meanpopM11, {{{1, 0, -4, -13, 0}, {0, 1, 4, 10, 0}, {0, 0, 0, 0, 1}}, "co"}];

(*Meantone⋎Marvel = 31*)
test2args[meet, meantoneC11, marvelC11, et31C11];

(*Meantone⋏Marvel = <225/224>*)
test2args[join, meantoneM11, marvelM11, dual[{{marvelComma11}, "contra"}]];

(*Meantone⋎Porcupine = G = <JI>*)
test2args[meet, meantoneC11, porcupineC11, {IdentityMatrix[5], "contra"}];

(*Meantone⋏Porcupine = <176/175>*)
test2args[join, meantoneM11, porcupineM11, dual[{{valinorsma11}, "contra"}]];

(*In the 7-limit, that become Meantone⋎Porcupine = <JI>, Meantone⋏Porcupine = <1>*)
test2args[meet, meantoneC7, porcupineC7, {IdentityMatrix[4], "contra"}];
test2args[join, meantoneM7, porcupineM7, {IdentityMatrix[4], "co"}];

(*Miracle⋎Magic = 41 *)
test2args[meet, miracleC11, magicC11, et41C11];

(*Miracle⋏Magic = Marvel *)
test2args[join, miracleM11, magicM11, marvelM11];

(*In the 7-limit, again Miracle⋎Magic = 41, Miracle⋏Magic = Marvel*)
test2args[meet, miracleC7, magicC7, et41C7];
test2args[join, miracleM7, magicM7, marvelM7];

(*Miracle⋎Mothra = 31 *)
test2args[meet, miracleC11, mothraC11, et31C11];

(* Miracle⋏Mothra = Portent *)
test2args[join, miracleM11, mothraM11, portentM11];

(*In the 7-limit, Miracle⋏Mothra = Gamelan.*)
test2args[join, miracleM7, mothraM7, gamelanM7];

(*Meantone⋎Magic = <JI>,*)
test2args[meet, meantoneC11, magicC11, {IdentityMatrix[5], "contra"}];

(*Meantone⋏Magic = <225/224>*)
test2args[join, meantoneM11, magicM11, dual[{{marvelComma11}, "contra"}]];



(* ___ PRIVATE ___ *)


(* LIST UTILITIES *)

(* divideOutGcd *)
test[divideOutGcd, {0, -6, 9}, {0, -2, 3}];
test[divideOutGcd, {-1, -2, -3}, {-1, -2, -3}];
test[divideOutGcd, {0, 0, 0}, {0, 0, 0}];

(* multByLcd *)
test[multByLcd, {1 / 3, 1, 2 / 5}, {5, 15, 6}];

(* leadingEntry *)
test[leadingEntry, {0, -6, 9, 0}, -6];

(* trailingEntry *)
test[trailingEntry, {0, -6, 9, 0}, 9];

(* allZerosL *)
test[allZerosL, {0, -6, 9}, False];
test[allZerosL, {0, 0, 0}, True];


(* MATRIX UTILITIES *)

(* allZeros *)
test[allZeros, {{1, 0, -4}, {0, 1, 4}}, False];
test[allZeros, {{0, 0, 0}, {0, 0, 0}}, True];

(* reverseEachRow *)
test[reverseEachRow, {{1, 0, -4}, {0, 1, 4}}, {{-4, 0, 1}, {4, 1, 0}}];

(* reverseEachCol *)
test[reverseEachCol, {{1, 0, -4}, {0, 1, 4}}, {{0, 1, 4}, {1, 0, -4}}];

(* antiTranspose *)
test[antiTranspose, {{1, 0, -4}, {0, 1, 4}}, {{4, 1, 0}, {-4, 0, 1}}];

(* removeAllZeroRows *)
test[removeAllZeroRows, {{1, 0, 0}, {0, 0, 0}, {1, 2, 3}}, {{1, 0, 0}, {1, 2, 3}}];
test[removeAllZeroRows, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}];
test[removeAllZeroRows, {{12, 19, 28}, {24, 38, 56}}, {{12, 19, 28}, {24, 38, 56}}];
test[removeAllZeroRows, {{0, 0}, {0, 0}}, {}];

(* removeUnneededZeroRows *)
test[removeUnneededZeroRows, {{1, 0, 0}, {0, 0, 0}, {1, 2, 3}}, {{1, 0, 0}, {1, 2, 3}}];
test[removeUnneededZeroRows, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}, {{1, 0, 1}, {0, 0, 2}, {0, 0, 3}}];
test[removeUnneededZeroRows, {{12, 19, 28}, {24, 38, 56}}, {{12, 19, 28}, {24, 38, 56}}];
test[removeUnneededZeroRows, {{0, 0}, {0, 0}}, {{0, 0}}];

(* colCount *)
test[colCount, {{0, 0}, {0, 0}}, 2];
test[colCount, {{0}, {0}}, 1];
test[colCount, {{0, 0}}, 2];


(* TEMPERAMENT UTILITIES *)

(* getA *)
test[getA, {{{1, 0, -4}, {0, 1, 4}}, "co"}, {{1, 0, -4}, {0, 1, 4}}];

(* getV *)
test[getV, {{{1, 0, -4}, {0, 1, 4}}, "co"}, "co"];

(* isContra *)
test[isContra, {{{1, 0, -4}, {0, 1, 4}}, "co"}, False];
test[isContra, {{{1, 2}, {3, 4}, {5, 6}}, "contra"}, True];
test[isContra, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "co"}, False];
test[isContra, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "contra"}, True];
test[isContra, {{{1, 2}, {0, 0}, {0, 0}}, "contra"}, True];
test[isContra, {{{1, 0, 0}, {2, 0, 0}}, "co"}, False];
test[isContra, {{{1, 0, -4}, {0, 1, 4}}, "mapping"}, False];
test[isContra, {{{1, 0, -4}, {0, 1, 4}}, "comma basis"}, True];

(* isCo *)
test[isCo, {{{1, 0, -4}, {0, 1, 4}}, "co"}, True];
test[isCo, {{{1, 2}, {3, 4}, {5, 6}}, "contra"}, False];
test[isCo, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "co"}, True];
test[isCo, {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "contra"}, False];
test[isCo, {{{1, 2}, {0, 0}, {0, 0}}, "contra"}, False];
test[isCo, {{{1, 0, 0}, {2, 0, 0}}, "co"}, True];
test[isCo, {{{1, 0, -4}, {0, 1, 4}}, "mapping"}, True];
test[isCo, {{{1, 0, -4}, {0, 1, 4}}, "comma basis"}, False];


(* CANONICALIZATION *)

(* hnf *)
test[hnf, {{5, 8, 12}, {7, 11, 16}}, {{1, 0, -4}, {0, 1, 4}}];
test[hnf, {{3, 0, -1}, {0, 3, 5}}, {{3, 0, -1}, {0, 3, 5}}];

(* hermiteRightUnimodular *)
test[hermiteRightUnimodular, {{6, 5, -4}, {4, -4, 1}}, {{1, 2, 1}, {-1, 0, 2}, {0, 3, 4}}];

(* colHermiteDefactor *)
test[colHermiteDefactor, {{6, 5, -4}, {4, -4, 1}}, {{6, 5, -4}, {-4, -4, 3}}];

(* canonicalM *)
test[canonicalM, {{1, 1, 0}, {0, 1, 4}}, {{1, 0, -4}, {0, 1, 4}}];

(* canonicalC *)
test[canonicalC, {{-4, 4, -1}}, {{4, -4, 1}}];


(* DUAL *)

(* noncanonicalNullSpaceBasis *)
test[noncanonicalNullSpaceBasis, {{19, 30, 44}}, {{-30, 19, 0}, {-44, 0, 19}}];

(* noncanonicalAntiNullSpaceBasis *)
test[noncanonicalAntiNullSpaceBasis, {{-30, 19, 0}, {-44, 0, 19}}, {{19, 30, 44}}];

(* nullSpaceBasis *)
test[nullSpaceBasis, {{1, 0, -4}, {0, 1, 4}}, {{4, -4, 1}}];
test[nullSpaceBasis, {{0, 9, 4}}, {{1, 0, 0}, {0, -4, 9}}];
test[nullSpaceBasis, {{0}}, IdentityMatrix[1]];
test[nullSpaceBasis, {{0, 0}}, IdentityMatrix[2]];
test[nullSpaceBasis, {{0, 0, 0}}, IdentityMatrix[3]];
test[nullSpaceBasis, IdentityMatrix[1], {{0}}];
test[nullSpaceBasis, IdentityMatrix[2], {{0, 0}}];
test[nullSpaceBasis, IdentityMatrix[3], {{0, 0, 0}}];
test[nullSpaceBasis, {{12, 19}}, {{-19, 12}}];

(* antiNullSpaceBasis *)
test[antiNullSpaceBasis, {{4, -4, 1}}, {{1, 0, -4}, {0, 1, 4}}];
test[antiNullSpaceBasis, {{1, 0, 0}, {0, -4, 9}}, {{0, 9, 4}}];
test[antiNullSpaceBasis, {{0}}, IdentityMatrix[1]];
test[antiNullSpaceBasis, {{0, 0}}, IdentityMatrix[2]];
test[antiNullSpaceBasis, {{0, 0, 0}}, IdentityMatrix[3]];
test[antiNullSpaceBasis, IdentityMatrix[1], {{0}}];
test[antiNullSpaceBasis, IdentityMatrix[2], {{0, 0}}];
test[antiNullSpaceBasis, IdentityMatrix[3], {{0, 0, 0}}];
test[antiNullSpaceBasis, {{-19, 12}}, {{12, 19}}];


(* MEET AND JOIN *)

(* getM *)
test[getM, {{{1, 0, -4}, {0, 1, 4}}, "co"}, {{1, 0, -4}, {0, 1, 4}}];
test[getM, {{{4, -4, 1}}, "contra"}, {{-1, 0, 4}, {1, 1, 0}}];

(* getC *)
test[getC, {{{1, 0, -4}, {0, 1, 4}}, "co"}, {{4, -4, 1}}];
test[getC, {{{4, -4, 1}}, "contra"}, {{4, -4, 1}}];




Print["TOTAL FAILURES: ", f];
Print["TOTAL PASSES: ", p];