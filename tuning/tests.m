failures = 0;
passes = 0;
accuracy = 3;

format = "EBK";

testClose[fn_, args___, inputExpectation_] := Module[
  {actual, expectation},
  
  (*Print["huhmmm", Apply[fn, {args}]];*)
  
  actual = First[getA[parseTemperamentData[Apply[fn, {args}]]]];
  expectation = First[getA[parseTemperamentData[inputExpectation]]];
  
  If[
    AllTrue[MapThread[Abs[#1 - #2] < 10^-accuracy&, {actual, expectation}], TrueQ],
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    printWrapper[ToString[SetAccuracy[actual, accuracy + 1]]];
  ]
];
testCloseNoParse[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    AllTrue[MapThread[Abs[#1 - #2] < 10^-accuracy&, {actual, expectation}], TrueQ],
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    printWrapper[ToString[SetAccuracy[actual, accuracy + 1]]];
  ]
];
testCloseNotList[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    Abs[actual - expectation] < 10^-accuracy,
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    printWrapper[ToString[SetAccuracy[actual, accuracy + 1]]];
  ]
];
testNotClose[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    AnyTrue[MapThread[Abs[#1 - #2] > 10^-accuracy&, {actual, expectation}], TrueQ],
    passes += 1,
    failures += 1;
    printWrapper[Style[StringForm["``[``] = `` but it was not supposed to", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
  ]
];


(* some temperaments to check against *)

meantone = "[⟨1 1 0] ⟨0 1 4]⟩";
blackwood = "[⟨5 8 0] ⟨0 0 1]⟩";
dicot = "[⟨1 1 2] ⟨0 2 1]⟩";
augmented = "[⟨3 0 7] ⟨0 1 0]⟩";
mavila = "[⟨1 0 7] ⟨0 1 -3]⟩";
porcupine = "[⟨1 2 3] ⟨0 3 5]⟩";
srutal = "[⟨2 0 11] ⟨0 1 -2]⟩";
hanson = "[⟨1 0 1] ⟨0 6 5]⟩";
magic = "[⟨1 0 2] ⟨0 5 1]⟩";
negri = "[⟨1 2 2] ⟨0 -4 3]⟩";
tetracot = "[⟨1 1 1] ⟨0 4 9]⟩";
meantone7 = "[⟨1 0 -4 -13] ⟨0 1 4 10]⟩";
magic7 = "[⟨1 0 2 -1] ⟨0 5 1 12]⟩";
pajara = "[⟨2 3 5 6] ⟨0 1 -2 -2]⟩";
augene = "[⟨3 0 7 18] ⟨0 1 0 -2]⟩";
sensi = "[⟨1 -1 -1 -2] ⟨0 7 9 13]⟩";
sensamagic = "[⟨1 0 0 0] ⟨0 1 1 2] ⟨0 0 2 -1]⟩";


(* optimizeGeneratorsTuningMap, using explicit targeted intervals *)

fiveOddLimitDiamond = "{3/2, 4/3, 5/4, 8/5, 5/3, 6/5}";

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted", "logging" -> True}, "⟨1200.000, 696.578]"];


(* optimizeGeneratorsTuningMap, by individual tuning scheme properties *)

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted"}, "⟨1200.000 696.578]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True}, "⟨1202.390 697.176]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2}, "⟨1202.728 697.260]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"}, "⟨1201.699 697.564]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2}, "⟨1201.600 697.531]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True}, "⟨1197.610 694.786]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2}, "⟨1197.435 694.976]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted"}, "⟨1197.979 694.711]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2}, "⟨1198.155 695.010]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "unweighted"}, "⟨1199.022 695.601]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True}, "⟨1200.070 696.005]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2}, "⟨1200.742 696.205]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted"}, "⟨1200.985 696.904]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2}, "⟨1201.127 696.905]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True}, "⟨1198.396 695.289]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2}, "⟨1198.244 695.294]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted"}, "⟨1198.085 694.930]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2}, "⟨1197.930 694.911]"];


testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "unweighted"}, "⟨1198.054 695.119]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True}, "⟨1195.699 693.352]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2}, "⟨1195.699 693.352]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted"}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2}, "⟨1200.000 696.578]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted"}, "⟨1195.699 693.352]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2}, "⟨1195.699 693.352]"];


(* optimizeGeneratorsTuningMap, fully by "systematicTuningSchemeName" *)

sevenOddLimitDiamond = "{3/2, 4/3, 5/4, 8/5, 5/3, 6/5, 7/4, 8/7, 7/6, 12/7, 7/5, 10/7, 9/8, 16/9, 9/5, 10/9, 9/7, 14/9}";

testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minimax-U", "⟨600.000 108.128]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minimax-copfr-S", "⟨596.502 106.058]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minimax-copfr-ES", "⟨598.233 106.938]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minimax-S", "⟨598.447 107.711]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minimax-ES", "⟨599.682 108.375]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minimax-copfr-C", "⟨601.515 108.014]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minimax-copfr-EC", "⟨601.826 108.325]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minimax-C", "⟨601.553 108.015]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minimax-EC", "⟨600.318 108.188]"];


testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisos-U", "⟨599.450 107.15]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisos-copfr-S", "⟨597.851 106.643]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisos-copfr-ES", "⟨598.310 106.798]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisos-S", "⟨598.436 106.672]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisos-ES", "⟨598.762 106.835]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisos-copfr-C", "⟨601.653 107.288]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisos-copfr-EC", "⟨601.522 107.178]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisos-C", "⟨600.655 107.426]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisos-EC", "⟨600.263 107.259]"];


testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisum-U", "⟨600.000 106.843]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisum-copfr-S", "⟨596.741 105.214]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisum-copfr-ES", "⟨596.741 105.214]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisum-S", "⟨596.741 105.214]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisum-ES", "⟨596.741 105.214]"];

testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisum-copfr-C", "⟨601.397 106.145]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisum-copfr-EC", "⟨601.397 106.145]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisum-C", "⟨600.000 106.843]"];
testClose[optimizeGeneratorsTuningMap, pajara, sevenOddLimitDiamond <> " minisum-EC", "⟨600.000 106.843]"];


(* optimizeGeneratorsTuningMap, by "systematicDamageName" plus traits 0 and 1 (targeted intervals, and optimization power) *)

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "systematicDamageName" -> "U-damage"}, "⟨600.000 1905.214]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "systematicDamageName" -> "copfr-S-damage"}, "⟨599.425 1903.105]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "systematicDamageName" -> "copfr-ES-damage"}, "⟨599.362 1902.875]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "systematicDamageName" -> "S-damage"}, "⟨599.555 1903.365]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "systematicDamageName" -> "ES-damage"}, "⟨599.577 1903.449]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "systematicDamageName" -> "copfr-C-damage"}, "⟨600.752 1907.971]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "systematicDamageName" -> "copfr-EC-damage"}, "⟨600.863 1908.379]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "systematicDamageName" -> "C-damage"}, "⟨600.626 1907.691]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "systematicDamageName" -> "EC-damage"}, "⟨600.587 1907.546]"];


testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "systematicDamageName" -> "U-damage"}, "⟨600.208 1906.115]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "systematicDamageName" -> "copfr-S-damage"}, "⟨599.398 1903.194]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "systematicDamageName" -> "copfr-ES-damage"}, "⟨599.333 1902.973]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "systematicDamageName" -> "S-damage"}, "⟨599.332 1902.950]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "systematicDamageName" -> "ES-damage"}, "⟨599.374 1903.116]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "systematicDamageName" -> "copfr-C-damage"}, "⟨600.886 1908.453]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "systematicDamageName" -> "copfr-EC-damage"}, "⟨600.988 1908.891]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "systematicDamageName" -> "C-damage"}, "⟨600.733 1907.946]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "systematicDamageName" -> "EC-damage"}, "⟨600.768 1908.116]"];


testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "systematicDamageName" -> "U-damage"}, "⟨600.000 1905.214]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "systematicDamageName" -> "copfr-S-damage"}, "⟨600.000 1905.214]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "systematicDamageName" -> "copfr-ES-damage"}, "⟨600.000 1905.214]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "systematicDamageName" -> "S-damage"}, "⟨600.000 1905.214]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "systematicDamageName" -> "ES-damage"}, "⟨600.000 1905.214]"];

testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "systematicDamageName" -> "copfr-C-damage"}, "⟨601.397 1910.335]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "systematicDamageName" -> "copfr-EC-damage"}, "⟨601.397 1910.335]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "systematicDamageName" -> "C-damage"}, "⟨601.397 1910.335]"];
testClose[optimizeGeneratorsTuningMap, srutal, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "systematicDamageName" -> "EC-damage"}, "⟨601.397 1910.335]"];


(* optimizeGeneratorsTuningMap, by "systematicComplexityName", plus traits 0, 1, and 2 (targeted intervals, optimization power, and damage weighting slope) *)

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted"}, "⟨240.000 2795.336]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "copfr-complexity"}, "⟨238.612 2784.000]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "copfr-E-complexity"}, "⟨238.445 2782.978]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "complexity"}, "⟨238.867 2784.297]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "E-complexity"}, "⟨238.927 2784.902]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "copfr-complexity"}, "⟨241.504 2812.629]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "copfr-E-complexity"}, "⟨241.702 2813.436]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "complexity"}, "⟨241.209 2809.927]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "E-complexity"}, "⟨241.140 2808.307]"];


testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "unweighted"}, "⟨240.430 2800.277]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "copfr-complexity"}, "⟨239.238 2790.669]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "copfr-E-complexity"}, "⟨238.949 2788.613]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "complexity"}, "⟨238.844 2783.616]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "E-complexity"}, "⟨238.846 2783.963]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "copfr-complexity"}, "⟨241.211 2807.746]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "copfr-E-complexity"}, "⟨241.340 2807.905]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "complexity"}, "⟨241.210 2809.203]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "E-complexity"}, "⟨241.298 2809.413]"];


testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "unweighted"}, "⟨242.578 2824.982]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "copfr-complexity"}, "⟨240.000 2804.359]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "copfr-E-complexity"}, "⟨240.000 2804.359]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "complexity"}, "⟨242.578 2824.982]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "systematicComplexityName" -> "E-complexity"}, "⟨242.578 2824.982]"];

testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "copfr-complexity"}, "⟨242.578 2824.982]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "copfr-E-complexity"}, "⟨242.578 2824.982]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "complexity"}, "⟨242.578 2824.982]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "systematicComplexityName" -> "E-complexity"}, "⟨242.578 2824.982]"];


(* optimization power continuum *)
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted"}, "⟨240.000 2795.336]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 5.00, "damageWeightingSlope" -> "unweighted"}, "⟨239.797 2793.002]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 3.00, "damageWeightingSlope" -> "unweighted"}, "⟨240.000 2795.336]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 2.00, "damageWeightingSlope" -> "unweighted"}, "⟨240.430 2800.277]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1.50, "damageWeightingSlope" -> "unweighted"}, "⟨241.129 2808.320]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1.25, "damageWeightingSlope" -> "unweighted"}, "⟨241.998 2818.309]"];
testClose[optimizeGeneratorsTuningMap, blackwood, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> 1.00, "damageWeightingSlope" -> "unweighted"}, "⟨242.578 2824.982]"];


(* complexity norm power continuum *)
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> \[Infinity]}, "⟨1201.191 697.405]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 5.00}, "⟨1201.381 697.460]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 3.00}, "⟨1201.513 697.503]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2.00}, "⟨1201.600 697.531]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 1.50}, "⟨1201.648 697.547]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 1.25}, "⟨1201.673 697.556]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> fiveOddLimitDiamond, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 1.00}, "⟨1201.699 697.564]"];


(* odd-diamond minimax-U = "minimax" *)
testClose[optimizeTuningMap, meantone, "odd-diamond minimax-U", "⟨1200.000 1896.578 2786.314]"]; (* [7a] *)
(* blackwood *)
(* dicot *)
(* augmented *)
(* mavila *)
testClose[optimizeGeneratorsTuningMap, porcupine, "odd-diamond minimax-U", "⟨1200.000 -162.737]"]; (* [7c] *)
(* srutal *)
(* hanson *)
testClose[optimizeGeneratorsTuningMap, magic, "odd-diamond minimax-U", "⟨1200.000 380.391]"]; (* [7d] *)
(* negri *)
testClose[optimizeGeneratorsTuningMap, tetracot, "odd-diamond minimax-U", "⟨1200.000 176.257]"]; (* [7e] *)
testClose[optimizeGeneratorsTuningMap, meantone7, "odd-diamond minimax-U", "⟨1200.000, 1200.000 + 696.578]"]; (* [7f] *)
testClose[optimizeGeneratorsTuningMap, magic7, "odd-diamond minimax-U", "⟨1200.00 380.391]"]; (* [7d] *)
(* pajara *)
accuracy = 1;
testClose[optimizeGeneratorsTuningMap, augene, "odd-diamond minimax-U", "⟨400.000, 3 * 400.000 + 708.798]"]; (* [7b] *)
accuracy = 3;
testClose[optimizeGeneratorsTuningMap, sensi, "odd-diamond minimax-U", "⟨1200.000 443.519]"]; (* [7g] *)
testClose[optimizeTuningMap, sensamagic, "odd-diamond minimax-U", "⟨1200.000 1901.955 2781.584 3364.096]"]; (* [7h] *)

(* odd-diamond minisos-U = "least squares" *)
testClose[optimizeGeneratorsTuningMap, meantone, "odd-diamond minisos-U", "⟨1200.000 696.165]"]; (* [7f] *)
(* blackwood *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
testClose[optimizeGeneratorsTuningMap, magic, "odd-diamond minisos-U", "⟨1200.000 379.968]"]; (* [7d]] *)
(* negri *)
(* tetracot *)
testClose[optimizeGeneratorsTuningMap, meantone7, "odd-diamond minisos-U", "⟨1200.000, 1200.000 + 696.436]"]; (* [7f] *)
testClose[optimizeGeneratorsTuningMap, magic7, "odd-diamond minisos-U", "⟨1200.000, 380.384]"]; (* [7d]] *)
(* pajara *)
(* augene *)
(* sensi *)
(* sensamagic *)
testClose[optimizeGeneratorsTuningMap, "[⟨1 0 15] ⟨0 1 -8]⟩", "odd-diamond minisos-U", "⟨1200.000, 1200.000 + 701.728]"]; (* [2b] has a bunch of least squares tunings... only this one works, though; not sure what's up with the rest. this is the temperament that tempers out 32805/32768, btw. *)


(* infinite-target-set tuning schemes *)
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True}, "⟨1202.390 697.176]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2}, "⟨1202.607 696.741]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"}, "⟨1201.699 697.564]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2}, "⟨1201.397 697.049]"];

testClose[optimizeGeneratorsTuningMap, pajara, {"targetedIntervals" -> {}, "systematicTuningSchemeName" -> "minimax-copfr-S"}, "⟨597.119 103.293]"];
testClose[optimizeGeneratorsTuningMap, pajara, {"targetedIntervals" -> {}, "systematicTuningSchemeName" -> "minimax-copfr-ES"}, "⟨598.345 106.693]"];
testClose[optimizeGeneratorsTuningMap, pajara, {"targetedIntervals" -> {}, "systematicTuningSchemeName" -> "minimax-S"}, "⟨598.447 106.567]"];
testClose[optimizeGeneratorsTuningMap, pajara, {"targetedIntervals" -> {}, "systematicTuningSchemeName" -> "minimax-ES"}, "⟨598.859 106.844]"];


(* interval basis *)
t = "2.7/5.11 [⟨1 1 5] ⟨0 -1 -3]⟩";
testClose[optimizeGeneratorsTuningMap, t, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningSchemeIntervalBasis" -> "formalPrimes"}, "⟨1200.4181 617.7581]"];
testClose[optimizeGeneratorsTuningMap, t, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningSchemeIntervalBasis" -> "primes"}, "⟨1200.0558 616.4318]"];

t = "2.9.5.21 [⟨1 0 -4 0] ⟨0 1 2 0] ⟨0 0 0 1]⟩";
testClose[optimizeGeneratorsTuningMap, t, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningSchemeIntervalBasis" -> "formalPrimes"}, "⟨1201.3969 3796.8919 5270.7809]"];
testClose[optimizeGeneratorsTuningMap, t, {"targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningSchemeIntervalBasis" -> "primes"}, "⟨1201.3969 3796.8919 5267.2719]"];


(* pure-stretched interval *)
testClose[optimizeGeneratorsTuningMap, meantone, fiveOddLimitDiamond <> " minisum-U", "⟨1198.054 695.119]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-U", "pureStretchedInterval" -> "octave"}, "⟨1198.054 * 1200.000 / 1198.054, 695.119 * 1200.000 / 1198.054]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-U", "pureStretchedInterval" -> "2"}, "⟨1198.054 * 1200.000 / 1198.054, 695.119 * 1200.000 / 1198.054]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-U", "pureStretchedInterval" -> "2/1"}, "⟨1198.054 * 1200.000 / 1198.054, 695.119 * 1200.000 / 1198.054]"];
testClose[optimizeGeneratorsTuningMap, meantone, "pure-stretched-octave " <> fiveOddLimitDiamond <> " minisum-U", "⟨1198.054 * 1200.000 / 1198.054, 695.119 * 1200.000 / 1198.054]"];
testClose[optimizeGeneratorsTuningMap, meantone, "pure-stretched-2 " <> fiveOddLimitDiamond <> " minisum-U", "⟨1198.054 * 1200.000 / 1198.054, 695.119 * 1200.000 / 1198.054]"];
testClose[optimizeGeneratorsTuningMap, meantone, "pure-stretched-2/1 " <> fiveOddLimitDiamond <> " minisum-U", "⟨1198.054 * 1200.000 / 1198.054, 695.119 * 1200.000 / 1198.054]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-U", "pureStretchedInterval" -> "3/2"}, "⟨1198.054 * 701.955 / 695.119, 695.119 * 701.955 / 695.119]"];
testClose[optimizeGeneratorsTuningMap, meantone, "pure-stretched-3/2 " <> fiveOddLimitDiamond <> " minisum-U", "⟨1198.054 * 701.955 / 695.119, 695.119 * 701.955 / 695.119]"];


(* unchanged interval *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-U", "unchangedIntervals" -> "octave"}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-U", "unchangedIntervals" -> "2"}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-U", "unchangedIntervals" -> "2/1"}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-U", "unchangedIntervals" -> "{2}"}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-U", "unchangedIntervals" -> "{2/1}"}, "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-octave " <> fiveOddLimitDiamond <> " minisum-U", "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-2 " <> fiveOddLimitDiamond <> " minisum-U", "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-2/1 " <> fiveOddLimitDiamond <> " minisum-U", "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-{2} " <> fiveOddLimitDiamond <> " minisum-U", "⟨1200.000 696.578]"];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-{2/1} " <> fiveOddLimitDiamond <> " minisum-U", "⟨1200.000 696.578]"];

testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-U", "unchangedIntervals" -> "{2/1, 3/2}"}, "⟨1200.000 701.955]"];
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-{2/1, 3/2} " <> fiveOddLimitDiamond <> " minisum-U", "⟨1200.000 701.955]"];


(* minimax-S = "TOP", "T1", "TOP-max", "TIPTOP", "Tenney OPtimal", "Tiebreaker-In-Polytope Tenney-OPtimal" *)
(* I had to fudge the factors to make mapping forms match in some places, due to rounding errors those matching factors introduced *)
(* could double-check with Scala, Xen wiki, Flora's app but it has incorrect results for TOP at this time *)
accuracy = 2;
testClose[optimizeGeneratorsTuningMap, meantone, "minimax-S", "⟨1201.70, 1201.70 - 504.13]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, blackwood, "minimax-S", "⟨238.87, 238.86 * 11.0003 + 158.78]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, dicot, "minimax-S", "⟨1207.66 353.22]"];(* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, augmented, "minimax-S", "⟨399.02, 399.018 * 5.00005 - 93.15]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, mavila, "minimax-S", "⟨1206.55, 1206.55 + 685.03]"];(* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, porcupine, "minimax-S", "⟨1196.91, 1034.59 - 1196.91]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, srutal, "minimax-S", "⟨599.56, 599.56 * 3.99999 - 494.86]"];(* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, hanson, "minimax-S", "⟨1200.29 317.07]"];(* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, magic, "minimax-S", "⟨1201.28 380.80]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, negri, "minimax-S", "⟨1201.82, 1201.82 - 1075.68]"]; (* [5] as "negripent" (Table 1) *)
testClose[optimizeGeneratorsTuningMap, tetracot, "minimax-S", "⟨1199.03 176.11]"]; (* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, meantone7, "minimax-S", "⟨1201.70, 1201.70 * 2 - 504.13]"]; (* [5](Table 2) *)
testClose[optimizeGeneratorsTuningMap, magic7, "minimax-S", "⟨1201.28 380.80]"]; (* [5] (Table 3) *)
testClose[optimizeGeneratorsTuningMap, pajara, "minimax-S", "⟨598.45, 598.45 - 491.88]"];  (* [5](Table 2) *)
testClose[optimizeGeneratorsTuningMap, augene, "minimax-S", "⟨399.02, 399.02 * 5 - 90.59]"]; (* [5] (Table 2) *)
testClose[optimizeGeneratorsTuningMap, sensi, "minimax-S", "⟨1198.39, 1198.39 - 755.23]"]; (* [5] as "sensisept" (Table 2) *)
testClose[optimizeTuningMap, meantone, "TOP", optimizeTuningMap[meantone, "minimax-S"]];
testClose[optimizeTuningMap, meantone, "T1", optimizeTuningMap[meantone, "minimax-S"]];
testClose[optimizeTuningMap, meantone, "TOP-max", optimizeTuningMap[meantone, "minimax-S"]];
testClose[optimizeTuningMap, meantone, "TIPTOP", optimizeTuningMap[meantone, "minimax-S"]];
accuracy = 3;

(* minimax-ES = "TE", "T2", "TOP-RMS", "Tenney-Euclidean" *)
(* could double-check with Scala, Sintel's app, Flora's app, and Xen wiki *)
testClose[optimizeTuningMap, meantone, "minimax-ES", "⟨1201.397 1898.446 2788.196]"]; (* [1a] *)
testClose[optimizeTuningMap, blackwood, "minimax-ES", "⟨1194.308 1910.892 2786.314]"]; (* [1a] *)
testClose[optimizeTuningMap, dicot, "minimax-ES", "⟨1206.410 1907.322 2763.276]"]; (* [3a] *)
testClose[optimizeTuningMap, augmented, "minimax-ES", "⟨1197.053 1901.955 2793.123]"]; (* [3b] *)
testClose[optimizeTuningMap, mavila, "minimax-ES", "⟨1208.380 1892.933 2779.860]"]; (* [3c] *)
testClose[optimizeTuningMap, porcupine, "minimax-ES", "⟨1199.562 1907.453 2779.234]"]; (* [3d] *)
testClose[optimizeTuningMap, srutal, "minimax-ES", "⟨1198.823 1903.030 2787.467]"]; (* [3e] *)
testClose[optimizeTuningMap, hanson, "minimax-ES", "⟨1200.166 1902.303 2785.418]"]; (* [3f] *)
testClose[optimizeTuningMap, magic, "minimax-ES", "⟨1201.248 1902.269 2782.950]"]; (* [3g] *)
testClose[optimizeTuningMap, negri, "minimax-ES", "⟨1202.347 1900.691 2782.698]"]; (* [3h] *)
testClose[optimizeTuningMap, tetracot, "minimax-ES", "⟨1199.561 1903.942 2784.419]"]; (* [3i] *)
testClose[optimizeTuningMap, meantone7, "minimax-ES", "⟨1201.242 1898.458 2788.863 3368.432]"]; (* [3j] *)
testClose[optimizeTuningMap, magic7, "minimax-ES", "⟨1201.082 1903.476 2782.860 3367.259]"]; (* [3k] *)
testClose[optimizeTuningMap, pajara, "minimax-ES", "⟨1197.719 1903.422 2780.608 3379.468]"]; (* [3l] *)
testClose[optimizeTuningMap, augene, "minimax-ES", "⟨1196.255 1903.298 2791.261 3370.933]"]; (* [3m] *)
testClose[optimizeTuningMap, sensi, "minimax-ES", "⟨1199.714 1903.225 2789.779 3363.173]"]; (* [3n] *)
testClose[optimizeTuningMap, sensamagic, "minimax-ES", "⟨1200.000 1903.742 2785.546 3366.583]"]; (* as "octorod" [3o] *)
testClose[optimizeGeneratorsTuningMap, meantone, "TE", optimizeGeneratorsTuningMap[meantone, "minimax-ES"]];
testClose[optimizeGeneratorsTuningMap, meantone, "T2", optimizeGeneratorsTuningMap[meantone, "minimax-ES"]];
testClose[optimizeGeneratorsTuningMap, meantone, "TOP-RMS", optimizeGeneratorsTuningMap[meantone, "minimax-ES"]];
testClose[optimizeGeneratorsTuningMap, meantone, "Tenney-Euclidean", optimizeGeneratorsTuningMap[meantone, "minimax-ES"]];

(* minimax-copfr-ES = "Frobenius" *)
(* could double-check with Scala, and Xen wiki *)
testClose[optimizeTuningMap, meantone, "minimax-copfr-ES", "⟨1202.6068 1899.3482 2786.9654]"]; (* [4] *)
testClose[optimizeTuningMap, blackwood, "minimax-copfr-ES", "⟨1191.8899 1907.0238 2786.3137]"]; (* [4] *)
testClose[optimizeTuningMap, dicot, "minimax-copfr-ES", "⟨1215.1441 1907.0030 2776.2177]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-copfr-ES", "⟨1195.0446 1901.9550 2788.4374]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "minimax-copfr-ES", "⟨1210.9365 1897.2679 2784.7514]"]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "minimax-copfr-ES", "⟨1198.5953 1908.9787 2782.0995]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-copfr-ES", "⟨1198.4746 1902.5097 2786.5911]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-copfr-ES", "⟨1200.5015 1902.3729 2785.8122]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-copfr-ES", "⟨1202.3503 1902.1900 2785.1386]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-copfr-ES", "⟨1203.2384 1901.2611 2785.3885]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "minimax-copfr-ES", "⟨1198.8664 1903.9955 2785.4068]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "minimax-copfr-ES", "⟨1201.3440 1898.5615 2788.8699 3368.1428]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "minimax-copfr-ES", "⟨1202.0285 1904.1849 2784.8940 3368.0151]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "minimax-copfr-ES", "⟨1196.6908 1901.7292 2778.3407 3376.6861]"]; (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-copfr-ES", "⟨1195.2617 1901.4887 2788.9439 3368.5928]"]; (* [4] *)
testClose[optimizeTuningMap, sensi, "minimax-copfr-ES", "⟨1198.2677 1904.0314 2790.4025 3364.8772]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "minimax-copfr-ES", "⟨1200.0000 1904.3201 2785.8407 3367.8799]"]; (* [4] *)
testClose[optimizeGeneratorsTuningMap, meantone, "Frobenius", optimizeGeneratorsTuningMap[meantone, "minimax-copfr-ES"]];

(* pure-stretched-octave minimax-ES = "POTE", "Pure Octave Tenney-Euclidean" *)
(* could double-check with Xen wiki *)
testClose[optimizeTuningMap, meantone, "pure-stretched-octave minimax-ES", "⟨1200.000 1896.239 2784.955]"]; (* [1a] *)
testClose[optimizeTuningMap, blackwood, "pure-stretched-octave minimax-ES", "⟨1200.000 1920.000 2799.594]"]; (* [1a] *)
testClose[optimizeTuningMap, dicot, "pure-stretched-octave minimax-ES", "⟨1200.000 1897.189 2748.594]"]; (* [3p] *)
testClose[optimizeTuningMap, augmented, "pure-stretched-octave minimax-ES", "⟨1200.000 1906.638 2800.000]"]; (* [3q] *)
testClose[optimizeTuningMap, mavila, "pure-stretched-octave minimax-ES", "⟨1200.000 1879.806 2760.582]"]; (* [3r] *)
testClose[optimizeTuningMap, porcupine, "pure-stretched-octave minimax-ES", "⟨1200.000 1908.149 2780.248]"]; (* [3s] *)
testClose[optimizeTuningMap, srutal, "pure-stretched-octave minimax-ES", "⟨1200.000 1904.898 2790.204]"]; (* [3t] *)
testClose[optimizeTuningMap, hanson, "pure-stretched-octave minimax-ES", "⟨1200.000 1902.039 2785.033]"]; (* [3u] *)
testClose[optimizeTuningMap, magic, "pure-stretched-octave minimax-ES", "⟨1200.000 1900.292 2780.058]"]; (* [3v] *)
testClose[optimizeTuningMap, negri, "pure-stretched-octave minimax-ES", "⟨1200.000 1896.980 2777.265]"]; (* [3w] *)
testClose[optimizeTuningMap, tetracot, "pure-stretched-octave minimax-ES", "⟨1200.000 1904.639 2785.438]"]; (* [3x] *)
testClose[optimizeTuningMap, meantone7, "pure-stretched-octave minimax-ES", "⟨1200.000 1896.495 2785.980 3364.949]"]; (* [3y] *)
testClose[optimizeTuningMap, magic7, "pure-stretched-octave minimax-ES", "⟨1200.000 1901.760 2780.352 3364.224]"]; (* [3z] *)
testClose[optimizeTuningMap, pajara, "pure-stretched-octave minimax-ES", "⟨1200.000 1907.048 2785.905 3385.905]"]; (* [3aa] *)
testClose[optimizeTuningMap, augene, "pure-stretched-octave minimax-ES", "⟨1200.000 1909.257 2800.000 3381.486]"]; (* [3ab] *)
testClose[optimizeTuningMap, sensi, "pure-stretched-octave minimax-ES", "⟨1200.000 1903.679 2790.444 3363.975]"]; (* [3ac] *)
testClose[optimizeTuningMap, sensamagic, "pure-stretched-octave minimax-ES", "⟨1200.000 1903.742 2785.546 3366.583]"]; (* as "octorod" [3ad] *)
testClose[optimizeGeneratorsTuningMap, meantone, "POTE", optimizeGeneratorsTuningMap[meantone, "pure-stretched-octave minimax-ES"]];

(* pure-stretched-octave minimax-S = "POTOP", "POTT", "Pure Octave Tenney OPtimal", "Pure Octave Tiebreaker-in-polytope Tenney-optimal" *)
(* could double-check against Flora's app, but her TOP results are incorrect for now, so these would be too *)
testClose[optimizeGeneratorsTuningMap, "[⟨2 2 7 8 14 5] ⟨0 1 -2 -2 -6 2]⟩", "pure-stretched-octave minimax-S", "⟨600.000 709.184]"]; (* [7j] has {600.000, 706.843} but that has 7.254 damage and mine has 5.988 *)
testClose[optimizeGeneratorsTuningMap, "[⟨1 -1 0 1] ⟨0 10 9 7]⟩", "pure-stretched-octave minimax-S", "⟨1200.000 310.196]"]; (* [7i] *)
accuracy = 1;
testClose[optimizeTuningMap, "[⟨1 3 0 0 3] ⟨0 -3 5 6 1]⟩", "pure-stretched-octave minimax-S", "⟨1200.00 1915.81 2806.98 3368.38 4161.40]"]; (* [1b] has <1200 1915.578 2807.355 3368.826 4161.472|,but  Mike himself says that maybe he got this one wrong because it should have been TIP... and yeah, I can see that this one has a pair of locked primes! *)
testClose[optimizeGeneratorsTuningMap, "[⟨1 2 6 2 10] ⟨0 -1 -9 2 -16]⟩", "pure-stretched-octave minimax-S", "⟨1200.0 490.4]"]; (* [1d] *)
testClose[optimizeGeneratorsTuningMap, "[⟨1 2 6 2 1] ⟨0 -1 -9 2 6]⟩", "pure-stretched-octave minimax-S", "⟨1200.0 490.9]"]; (* [1d] *)
testClose[optimizeGeneratorsTuningMap, "[⟨1 2 -3 2 1] ⟨0 -1 13 2 6]⟩", "pure-stretched-octave minimax-S", "⟨1200.0 491.9]"]; (* [1d] *)
accuracy = 3;
testClose[optimizeGeneratorsTuningMap, "[⟨1 1 2 1] ⟨0 1 0 2] ⟨0 0 1 2]⟩", "pure-stretched-octave minimax-S", "⟨1200.0 700.3907806 384.0221726]"]; (* [1e] this was passing with {1200.000, 700.795, 380.759} before introducing the non-unique check code and then went back to passing after maybe switching to Keenan's nested minimax technique...  it really does seem like it should have a unique solution, so the condition on that might be wrong... you should really plot this one visually and see what's happening *)
accuracy = 2;
testClose[optimizeGeneratorsTuningMap, "[⟨1 1 0] ⟨0 1 4]⟩", "pure-stretched-octave minimax-S", "⟨1200.0 696.58]"]; (* [1f] *)
testClose[optimizeGeneratorsTuningMap, "[⟨1 1 0 -3] ⟨0 1 4 10]⟩", "pure-stretched-octave minimax-S", "⟨1200.0 696.58]"]; (* [1f] *)
accuracy = 3;
testClose[optimizeTuningMap, meantone, "POTOP", optimizeTuningMap[meantone, "pure-stretched-octave minimax-S"]];
testClose[optimizeTuningMap, meantone, "POTT", optimizeTuningMap[meantone, "pure-stretched-octave minimax-S"]];

(* minimax-sopfr-S = "BOP", "Benedetti OPtimal" *)
testClose[optimizeTuningMap, meantone, "minimax-sopfr-S", "⟨1201.721 1899.374 2790.615]"];  (* [4] *)
testClose[optimizeTuningMap, blackwood, "minimax-sopfr-S", "⟨1194.179 1910.686 2786.314]"];  (* [4] has ⟨1194.179 1910.6865 2788.2941] which has the same damage, but prime 5 might as well be tuned pure *)
testClose[optimizeTuningMap, dicot, "minimax-sopfr-S", "⟨1207.439 1913.114 2767.716]"]; (* [4] has ⟨1207.4442 1913.0740 2767.7033], but that has 3.722 damage and mine has 3.720 *)
testClose[optimizeTuningMap, augmented, "minimax-sopfr-S", "⟨1197.168 1901.955 2793.393]"];  (* [4] has ⟨1197.1684 1898.1244 2793.3928] which has the same damage, but prime 3 might as well be tuned pure *)
testClose[optimizeTuningMap, mavila, "minimax-sopfr-S", "⟨1206.584 1892.079 2769.853]"];  (* [4] has ⟨1206.6238 1892.2042 2769.7542], but that has 3.312 damage and mine has 3.292 *)
testClose[optimizeTuningMap, porcupine, "minimax-sopfr-S", "⟨1196.9271 1906.5643 2778.6315]"];  (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-sopfr-S", "⟨1199.1112 1903.2881 2788.5356]"];  (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-sopfr-S", "⟨1200.2845 1902.3817 2785.6025]"];  (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-sopfr-S", "⟨1201.2338 1903.8059 2783.2287]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-sopfr-S", "⟨1201.7937 1899.2646 2781.8295]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "minimax-sopfr-S", "⟨1199.029 1903.411 2783.887]"];  (* [4] has ⟨1199.0355 1903.4127 2783.8842] which has 0.486 damage but mine has 0.485 *)
testClose[optimizeTuningMap, meantone7, "minimax-sopfr-S", "⟨1201.721 1899.374 2790.615 3371.376]"]; (* [4] has ⟨1202.0696 1898.8506 2787.1243 3361.6020], but that has 1.035 damage and mine has 0.860 damage *)
testClose[optimizeTuningMap, magic7, "minimax-sopfr-S", "⟨1201.234 1903.806 2783.229 3367.900]"];  (* [4] has  ⟨1201.2364 1903.8094 2783.2346 3367.9063], but that has 0.618 damage and mine has 0.617 *)
testClose[optimizeTuningMap, pajara, "minimax-sopfr-S", "⟨1197.3094 1902.8073 2779.5873 3378.2420]"];  (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-sopfr-S", "⟨1197.168 1904.326 2793.393 3374.358]"];  (* [4] has ⟨1197.1684 1902.1518 2793.3928 3378.7064] which has the same damage, but it can be visualized with graphTuningDamage[augene, "minimax-sopfr-S"] that mine does a nested minimax, minimizing the maximum damage between primes 3 and 7 underneath the minimax boundary between primes 2 and 5 *)
testClose[optimizeTuningMap, sensi, "minimax-sopfr-S", "⟨1198.5891 1903.5233 2789.8411 3363.8876]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "minimax-sopfr-S", "⟨1200.0000 1903.2071 2784.2268 3365.9044]"]; (* [4] *)
testClose[optimizeGeneratorsTuningMap, meantone, "BOP", optimizeGeneratorsTuningMap[meantone, "minimax-sopfr-S"]];

(* minimax-sopfr-ES = "BE", "Benedetti-Euclidean" *)
testClose[optimizeTuningMap, meantone, "minimax-sopfr-ES", "⟨1201.4768 1898.6321 2788.6213]"]; (* [4] *)
testClose[optimizeTuningMap, blackwood, "minimax-sopfr-ES", "⟨1193.9975 1910.3960 2786.3137]"]; (* [4] *)
testClose[optimizeTuningMap, dicot, "minimax-sopfr-ES", "⟨1205.8488 1906.3416 2761.9439]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-sopfr-ES", "⟨1197.2692 1901.9550 2793.6282]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "minimax-sopfr-ES", "⟨1208.5464 1893.7139 2778.683 ]"]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "minimax-sopfr-ES", "⟨1199.5668 1906.8283 2778.1916]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-sopfr-ES", "⟨1198.8183 1902.9219 2787.6566]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-sopfr-ES", "⟨1200.1533 1902.2425 2785.3554]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-sopfr-ES", "⟨1201.1456 1902.2128 2782.7337]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-sopfr-ES", "⟨1202.2630 1900.8639 2782.2726]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "minimax-sopfr-ES", "⟨1199.5499 1903.7780 2784.0631]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "minimax-sopfr-ES", "⟨1201.3847 1898.6480 2789.0531 3368.4787]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "minimax-sopfr-ES", "⟨1200.9990 1903.1832 2782.6345 3366.6407]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "minimax-sopfr-ES", "⟨1197.9072 1903.2635 2781.9626 3380.9162]"]; (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-sopfr-ES", "⟨1196.4076 1903.1641 2791.6178 3372.1175]"]; (* [4] *)
testClose[optimizeTuningMap, sensi, "minimax-sopfr-ES", "⟨1199.7904 1902.7978 2789.2516 3362.3687]"]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "minimax-sopfr-ES", "⟨1200.0000 1903.3868 2785.5183 3365.7078]"]; (* [4] *)
testClose[optimizeGeneratorsTuningMap, meantone, "BE", optimizeGeneratorsTuningMap[meantone, "minimax-sopfr-ES"]];
testClose[optimizeGeneratorsTuningMap, meantone, "Benedetti-Euclidean", optimizeGeneratorsTuningMap[meantone, "minimax-sopfr-ES"]];

(* minimax-lil-S = "Weil" *)
(* could maybe double-check w/ Flora's app but we're aware at this time that her implementation uses the pseudoinverse
of the Weil complexity multiplier which doesn't work correctly *)
testClose[optimizeTuningMap, meantone, "minimax-lil-S", "⟨1200.000 1896.578 2786.314]"]; (* [2a] *)
testClose[optimizeTuningMap, blackwood, "minimax-lil-S", "⟨1188.722 1901.955 2773.22]"]; (* [2a] *)
testClose[optimizeTuningMap, dicot, "minimax-lil-S", "⟨1200.000 1901.955 2750.978]"]; (* [2a] *)
testClose[optimizeTuningMap, augmented, "minimax-lil-S", "⟨1194.134 1897.307 2786.314]"]; (* [2a] *)
testClose[optimizeTuningMap, mavila, "minimax-lil-S", "⟨1200.000 1881.31 2756.07]"]; (* [2a] *)
testClose[optimizeTuningMap, porcupine, "minimax-lil-S", "⟨1193.828 1901.955 2771.982]"]; (* [2a] *)
testClose[optimizeTuningMap, srutal, "minimax-lil-S", "⟨1198.222 1901.955 2786.314]"]; (* [2a] *)
testClose[optimizeTuningMap, hanson, "minimax-lil-S", "⟨1200.000 1901.955 2784.963]"]; (* [2a] *)
testClose[optimizeTuningMap, magic, "minimax-lil-S", "⟨1200.000 1901.955 2780.391]"]; (* [2a] *)
testClose[optimizeTuningMap, negri, "minimax-lil-S", "⟨1200.000 1896.185 2777.861]"]; (* [2a] *)
testClose[optimizeTuningMap, tetracot, "minimax-lil-S", "⟨1198.064 1901.955 2781.819]"]; (* [2a] *)
testClose[optimizeTuningMap, meantone7, "minimax-lil-S", "⟨1200.000 1896.578 2786.314 3365.784]"]; (* [2a] *)
testClose[optimizeTuningMap, magic7, "minimax-lil-S", "⟨1200.000 1901.955 2780.391 3364.692]"]; (* [2a] *)
testClose[optimizeTuningMap, pajara, "minimax-lil-S", "⟨1193.803 1896.996 2771.924 3368.826]"]; (* [2a] *)
testClose[optimizeTuningMap, augene, "minimax-lil-S", "⟨1194.134 1899.852 2786.314 3365.102]"]; (* [2a] *)
testClose[optimizeTuningMap, sensi, "minimax-lil-S", "⟨1196.783 1901.181 2786.314 3359.796]"]; (* [2a] *)
testClose[optimizeGeneratorsTuningMap, meantone, "Weil", optimizeGeneratorsTuningMap[meantone, "minimax-lil-S"]];

(* minimax-lil-ES = "WE", "Weil-Euclidean" *)
(* could maybe double check w/ Sintel's app; what he calls Weil is actually Weil-Euclidean, according to Tom here: [10a] and I think he's right 
but unfortunately it's not easily discernible from his code at this time *)
testClose[optimizeTuningMap, meantone, "minimax-lil-ES", "⟨1201.3906 1898.4361 2788.1819]"]; (* [4] and [1a] also has ⟨1201.391 1898.436 2788.182] *)
testClose[optimizeTuningMap, blackwood, "minimax-lil-ES", "⟨1194.2544 1910.8071 2786.1895]"]; (* [4] and [1a] also has ⟨1194.254 1910.807 2786.189] *)
testClose[optimizeTuningMap, dicot, "minimax-lil-ES", "⟨1206.2832 1907.1223 2762.9860]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-lil-ES", "⟨1197.0385 1901.9322 2793.0898]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "minimax-lil-ES", "⟨1208.2873 1892.7881 2779.6466]"]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "minimax-lil-ES", "⟨1199.5444 1907.4244 2779.1926]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-lil-ES", "⟨1198.8214 1903.0273 2787.4633]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-lil-ES", "⟨1200.1659 1902.3024 2785.4179]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-lil-ES", "⟨1201.2449 1902.2636 2782.9425]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-lil-ES", "⟨1202.3403 1900.6800 2782.6811]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "minimax-lil-ES", "⟨1199.5586 1903.9387 2784.4138]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "minimax-lil-ES", "⟨1201.2358, 1898.4479, 2788.8486, 3368.4143]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "minimax-lil-ES", "⟨1201.0786, 1903.4695, 2782.8510, 3367.2482]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "minimax-lil-ES", "⟨1197.6967, 1903.3872, 2780.5573, 3379.4056]"]; (* [4] *)
testClose[optimizeTuningMap, augene, "minimax-lil-ES", "⟨1196.2383, 1903.2719, 2791.2228, 3370.8863]"]; (* [4] *)
testClose[optimizeTuningMap, sensi, "minimax-lil-ES", "⟨1199.7081, 1903.2158, 2789.7655, 3363.1568]"]; (* [4] *)
testClose[optimizeGeneratorsTuningMap, meantone, "WE", optimizeGeneratorsTuningMap[meantone, "minimax-lil-ES"]];
testClose[optimizeGeneratorsTuningMap, meantone, "Weil-Euclidean", optimizeGeneratorsTuningMap[meantone, "minimax-lil-ES"]];

(* minimax-lol-S = "Kees" *)
(* could maybe double-check with Flora's app, but per comment above about her implementation of Weil, we know it won't match now *)
(* this is the only actual example of a Kees tuning ever stated publicly by a human *)
(*accuracy = 1;
testClose[optimizeTuningMap, "[⟨1 3 0 0 3] ⟨0 -3 5 6 1]⟩", "minimax-lol-S", "⟨1200.00 1915.93 2806.79 3368.14 4161.36]"]; (* [1b] *)
accuracy = 3;
testClose[optimizeGeneratorsTuningMap, meantone, "Kees", optimizeGeneratorsTuningMap[meantone, "minimax-lol-S"]];*)

(* minimax-lol-ES = "KE", "Kees-Euclidean" *)
(* may be able double-check w/ Sintel's app; should support it by octave-constraining WE, once that's figured out. see above *)
(*accuracy = 2;
testClose[optimizeTuningMap, meantone, "minimax-lol-ES", "⟨1200.000, 1896.651, 2786.6064]"]; (* [4]; [1a] has ⟨1200.000 1896.651 2786.605] *)
testClose[optimizeTuningMap, blackwood, "minimax-lol-ES", "⟨1200.0000 1920.0000 2795.1253]"]; (* [4]; [1a] has ⟨1200.000 1920.000 2795.126] *)
testClose[optimizeTuningMap, dicot, "minimax-lol-ES", "⟨1200.0000 1902.1713 2751.0856]"]; (* [4] *)
testClose[optimizeTuningMap, augmented, "minimax-lol-ES", "⟨1200.0000 1905.0702 2800.0000]"]; (* [4] *)
testClose[optimizeTuningMap, mavila, "minimax-lol-ES", "⟨1200.0000 1879.1117 2762.6648]"]; (* [4] *)
accuracy = 3;
testClose[optimizeTuningMap, porcupine, "minimax-lol-ES", "⟨1200.0000 1907.8136 2779.6894]"]; (* [4] *)
testClose[optimizeTuningMap, srutal, "minimax-lol-ES", "⟨1200.0000 1904.9585 2790.0830]"]; (* [4] *)
testClose[optimizeTuningMap, hanson, "minimax-lol-ES", "⟨1200.0000 1902.1850 2785.1542]"]; (* [4] *)
testClose[optimizeTuningMap, magic, "minimax-lol-ES", "⟨1200.0000 1901.0972 2780.2194]"]; (* [4] *)
testClose[optimizeTuningMap, negri, "minimax-lol-ES", "⟨1200.0000 1897.3560 2776.9830]"]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "minimax-lol-ES", "⟨1200.0000 1904.3859 2784.8683]"]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "minimax-lol-ES", "⟨1200.0000 1896.6562 2786.6248 3366.562]"]; (* [4] *)
testClose[optimizeTuningMap, magic7, "minimax-lol-ES", "⟨1200.0000 1902.2878 2780.4576 3365.4906]"]; (* [4] *)
testClose[optimizeTuningMap, pajara, "minimax-lol-ES", "⟨1200.0000 1907.3437 2785.3126 3385.3126]"]; (* [4] *)
accuracy = 2;
testClose[optimizeTuningMap, augene, "minimax-lol-ES", "⟨1200.0000 1909.3238 2800.0000 3381.3524]"]; (* [4] *)
accuracy = 3;
testClose[optimizeTuningMap, sensi, "minimax-lol-ES", "⟨1200.0000 1903.4449 2790.1435 3363.5406]"]; (* [4] *)
testClose[optimizeGeneratorsTuningMap, meantone, "KE", optimizeGeneratorsTuningMap[meantone, "minimax-lol-ES"]];
testClose[optimizeGeneratorsTuningMap, meantone, "Kees-Euclidean", optimizeGeneratorsTuningMap[meantone, "minimax-lol-ES"]];*)

(* unchanged-octave minimax-ES = "CTE", "Constrained Tenney-Euclidean" *)
testClose[optimizeGeneratorsTuningMap, meantone, "unchanged-octave minimax-ES", "⟨1200.000 697.214]"]; (* [8a] *)
testClose[optimizeGeneratorsTuningMap, blackwood, "unchanged-octave minimax-ES", "⟨240.000, 1200.000 * 2 + 386.314]"]; (* [8b] *)
testClose[optimizeGeneratorsTuningMap, dicot, "unchanged-octave minimax-ES", "⟨1200.000 354.664]"]; (* [8c] *)
testClose[optimizeGeneratorsTuningMap, augmented, "unchanged-octave minimax-ES", "⟨400.000, 1200.000 + 701.955]"]; (* [8d] *)
testClose[optimizeGeneratorsTuningMap, mavila, "unchanged-octave minimax-ES", "⟨1200.000, 1200.000 + 677.145]"]; (* [8e] *)
testClose[optimizeGeneratorsTuningMap, porcupine, "unchanged-octave minimax-ES", "⟨1200.000 -164.166]"]; (* [8f] *)
testClose[optimizeGeneratorsTuningMap, srutal, "unchanged-octave minimax-ES", "⟨600.000, 1200.000 + 705.136]"]; (* [8g] *)
testClose[optimizeGeneratorsTuningMap, hanson, "unchanged-octave minimax-ES", "⟨1200.000 317.059]"]; (* [8h] *)
testClose[optimizeGeneratorsTuningMap, magic, "unchanged-octave minimax-ES", "⟨1200.000 380.499]"]; (* [8i] *)
testClose[optimizeGeneratorsTuningMap, negri, "unchanged-octave minimax-ES", "⟨1200.000 125.396]"]; (* [8j] *)
testClose[optimizeGeneratorsTuningMap, tetracot, "unchanged-octave minimax-ES", "⟨1200.000 176.028]"]; (* [8k] *)
testClose[optimizeGeneratorsTuningMap, meantone7, "unchanged-octave minimax-ES", "⟨1200.000, 1200.000 + 696.952]"]; (* [8l] *)
testClose[optimizeGeneratorsTuningMap, magic7, "unchanged-octave minimax-ES", "⟨1200.000 380.651]"]; (* [8m] *)
testClose[optimizeGeneratorsTuningMap, pajara, "unchanged-octave minimax-ES", "⟨600.000, 600.000 * -1 + 708.356]"]; (* [8n] *)
testClose[optimizeGeneratorsTuningMap, augene, "unchanged-octave minimax-ES", "⟨400.000, 1200.000 + 709.595]"]; (* [8o] *)
testClose[optimizeGeneratorsTuningMap, sensi, "unchanged-octave minimax-ES", "⟨1200.000, 1200.000 - 756.683]"]; (* [8p] *)
testClose[optimizeGeneratorsTuningMap, sensamagic, "unchanged-octave minimax-ES", "⟨1200.000, 1200.000 + 703.742, 440.902]"]; (* [8q] *)
testClose[optimizeTuningMap, meantone, "CTE", optimizeTuningMap[meantone, "unchanged-octave minimax-ES"]];
testClose[optimizeTuningMap, meantone, "Constrained Tenney-Euclidean", optimizeTuningMap[meantone, "unchanged-octave minimax-ES"]];


(* I no longer really care about tuning scheme equivalences 
such as minimax-lol-S w/ pure-stretched-octave minimax-S ("Kees" w/ "POTOP")
or minimax-lol-ES w/ pure-stretched-octave minimax-ES ("KE" w/ "POTE")
clearly minimax-lol-S is the same as pure-*constrained*-octave (unchanged-octave) minimax-S ("Kees" w/ pure-*constrained*-octave (unchanged-octave) "TOP")
and minimax-lol-ES is the same as pure-*constrained*-octave (unchanged-octave) minimax-ES ("KE" w/ pure-*constrained*-octave (unchanged-octave) "TE")
otherwise... who really cares? *)


(* confirming the relationship between log integer limit and log product tuning schemes for various target sets and optimization powers *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minimax-lil-S"}, "⟨1201.191 697.405]"];                               (* lil     / finite    / max *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-lil-S"}, "⟨1200.000 696.578]"];                               (* lil     / finite    / sum *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisos-lil-S"}, "⟨1200.665 696.707]"];                               (* lil     / finite    / sos *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisop-lil-S", "optimizationPower" -> 3}, "⟨1201.119 697.031]"];     (* lil     / finite    / sop *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-lil-S"}, "⟨1200.000 696.578]"];                                       (* lil     / infinite  / max *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-lil-S", "complexityNormPower" -> \[Infinity]}, "⟨1200.000 696.578]"];           (* lil     / infinite  / sum *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-lil-ES"}, "⟨1201.391 697.045]"];                                      (* lil     / infinite  / sos *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-lil-S", "complexityNormPower" -> 3}, "⟨1201.038 696.782]"];           (* lil     / infinite  / sop *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minimax-S"}, "⟨1201.699 697.564]"];                                   (* not-lil / finite    / max *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisum-S"}, "⟨1200.000 696.578]"];                                   (* not-lil / finite    / sum *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisos-S"}, "⟨1200.985 696.904]"];                                   (* not-lil / finite    / sos *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> fiveOddLimitDiamond <> " minisop-S", "optimizationPower" -> 3}, "⟨1201.476 697.233]"];         (* not-lil / finite    / sop*)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-S"}, "⟨1201.699 697.564]"];                                           (* not-lil / infinite  / max *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-S", "complexityNormPower" -> \[Infinity]}, "⟨1200.000 696.578]"];               (* not-lil / infinite  / sum *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-S"}, "⟨1201.699 697.564]"];                                           (* not-lil / infinite  / sos *)
testClose[optimizeGeneratorsTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-S", "complexityNormPower" -> 3}, "⟨1201.039 696.782]"];               (* not-lil / infinite  / sop *)


(* continuum between minimax-S (Mike's k = 0) and minimax-lil-S (Mike's k = 1) as well as beyond (k > 1) *)
testClose[optimizeTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-S", "complexitySizeFactor" -> 0.00}, "⟨1201.699 1899.263 2790.258]"];
testClose[optimizeTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-S", "complexitySizeFactor" -> 0.25}, "⟨1201.273 1898.591 2789.271]"];
testClose[optimizeTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-S", "complexitySizeFactor" -> 0.50}, "⟨1200.849 1897.920 2788.284]"];
testClose[optimizeTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-S", "complexitySizeFactor" -> 1.00}, "⟨1200.000 1896.578 2786.314]"];
testClose[optimizeTuningMap, meantone, {"systematicTuningSchemeName" -> "minimax-S", "complexitySizeFactor" -> 2.00}, "⟨1198.306 1893.902 2782.381]"];


(* proving that minimax-ES = primes minisos-S *)
testClose[optimizeGeneratorsTuningMap, meantone, "minimax-ES", optimizeGeneratorsTuningMap[meantone, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, blackwood, "minimax-ES", optimizeGeneratorsTuningMap[blackwood, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, dicot, "minimax-ES", optimizeGeneratorsTuningMap[dicot, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, augmented, "minimax-ES", optimizeGeneratorsTuningMap[augmented, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, mavila, "minimax-ES", optimizeGeneratorsTuningMap[mavila, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, porcupine, "minimax-ES", optimizeGeneratorsTuningMap[porcupine, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, srutal, "minimax-ES", optimizeGeneratorsTuningMap[srutal, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, hanson, "minimax-ES", optimizeGeneratorsTuningMap[hanson, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, magic, "minimax-ES", optimizeGeneratorsTuningMap[magic, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, negri, "minimax-ES", optimizeGeneratorsTuningMap[negri, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, tetracot, "minimax-ES", optimizeGeneratorsTuningMap[tetracot, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, meantone7, "minimax-ES", optimizeGeneratorsTuningMap[meantone7, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, magic7, "minimax-ES", optimizeGeneratorsTuningMap[magic7, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, pajara, "minimax-ES", optimizeGeneratorsTuningMap[pajara, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, augene, "minimax-ES", optimizeGeneratorsTuningMap[augene, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, sensi, "minimax-ES", optimizeGeneratorsTuningMap[sensi, "primes minisos-S"]];
testClose[optimizeGeneratorsTuningMap, sensamagic, "minimax-ES", optimizeGeneratorsTuningMap[sensamagic, "primes minisos-S"]];

(* proving that minimax-copfr-ES = primes minisos-U *)
testClose[optimizeGeneratorsTuningMap, meantone, "minimax-copfr-ES", optimizeGeneratorsTuningMap[meantone, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, blackwood, "minimax-copfr-ES", optimizeGeneratorsTuningMap[blackwood, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, dicot, "minimax-copfr-ES", optimizeGeneratorsTuningMap[dicot, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, augmented, "minimax-copfr-ES", optimizeGeneratorsTuningMap[augmented, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, mavila, "minimax-copfr-ES", optimizeGeneratorsTuningMap[mavila, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, porcupine, "minimax-copfr-ES", optimizeGeneratorsTuningMap[porcupine, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, srutal, "minimax-copfr-ES", optimizeGeneratorsTuningMap[srutal, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, hanson, "minimax-copfr-ES", optimizeGeneratorsTuningMap[hanson, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, magic, "minimax-copfr-ES", optimizeGeneratorsTuningMap[magic, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, negri, "minimax-copfr-ES", optimizeGeneratorsTuningMap[negri, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, tetracot, "minimax-copfr-ES", optimizeGeneratorsTuningMap[tetracot, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, meantone7, "minimax-copfr-ES", optimizeGeneratorsTuningMap[meantone7, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, magic7, "minimax-copfr-ES", optimizeGeneratorsTuningMap[magic7, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, pajara, "minimax-copfr-ES", optimizeGeneratorsTuningMap[pajara, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, augene, "minimax-copfr-ES", optimizeGeneratorsTuningMap[augene, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, sensi, "minimax-copfr-ES", optimizeGeneratorsTuningMap[sensi, "primes minisos-U"]];
testClose[optimizeGeneratorsTuningMap, sensamagic, "minimax-copfr-ES", optimizeGeneratorsTuningMap[sensamagic, "primes minisos-U"]];

(* proving that minimax-S = primes minimax-S *)
testClose[optimizeGeneratorsTuningMap, meantone, "minimax-S", optimizeGeneratorsTuningMap[meantone, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, blackwood, "minimax-S", optimizeGeneratorsTuningMap[blackwood, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, dicot, "minimax-S", optimizeGeneratorsTuningMap[dicot, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, augmented, "minimax-S", optimizeGeneratorsTuningMap[augmented, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, mavila, "minimax-S", optimizeGeneratorsTuningMap[mavila, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, porcupine, "minimax-S", optimizeGeneratorsTuningMap[porcupine, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, srutal, "minimax-S", optimizeGeneratorsTuningMap[srutal, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, hanson, "minimax-S", optimizeGeneratorsTuningMap[hanson, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, magic, "minimax-S", optimizeGeneratorsTuningMap[magic, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, negri, "minimax-S", optimizeGeneratorsTuningMap[negri, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, tetracot, "minimax-S", optimizeGeneratorsTuningMap[tetracot, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, meantone7, "minimax-S", optimizeGeneratorsTuningMap[meantone7, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, magic7, "minimax-S", optimizeGeneratorsTuningMap[magic7, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, pajara, "minimax-S", optimizeGeneratorsTuningMap[pajara, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, augene, "minimax-S", optimizeGeneratorsTuningMap[augene, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, sensi, "minimax-S", optimizeGeneratorsTuningMap[sensi, "primes minimax-S"]];
testClose[optimizeGeneratorsTuningMap, sensamagic, "minimax-S", optimizeGeneratorsTuningMap[sensamagic, "primes minimax-S"]];

(* proving that minimax-copfr-S = primes minimax-U *)
testClose[optimizeGeneratorsTuningMap, meantone, "minimax-copfr-S", optimizeGeneratorsTuningMap[meantone, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, blackwood, "minimax-copfr-S", optimizeGeneratorsTuningMap[blackwood, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, dicot, "minimax-copfr-S", optimizeGeneratorsTuningMap[dicot, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, augmented, "minimax-copfr-S", optimizeGeneratorsTuningMap[augmented, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, mavila, "minimax-copfr-S", optimizeGeneratorsTuningMap[mavila, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, porcupine, "minimax-copfr-S", optimizeGeneratorsTuningMap[porcupine, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, srutal, "minimax-copfr-S", optimizeGeneratorsTuningMap[srutal, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, hanson, "minimax-copfr-S", optimizeGeneratorsTuningMap[hanson, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, magic, "minimax-copfr-S", optimizeGeneratorsTuningMap[magic, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, negri, "minimax-copfr-S", optimizeGeneratorsTuningMap[negri, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, tetracot, "minimax-copfr-S", optimizeGeneratorsTuningMap[tetracot, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, meantone7, "minimax-copfr-S", optimizeGeneratorsTuningMap[meantone7, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, magic7, "minimax-copfr-S", optimizeGeneratorsTuningMap[magic7, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, pajara, "minimax-copfr-S", optimizeGeneratorsTuningMap[pajara, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, augene, "minimax-copfr-S", optimizeGeneratorsTuningMap[augene, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, sensi, "minimax-copfr-S", optimizeGeneratorsTuningMap[sensi, "primes minimax-U"]];
testClose[optimizeGeneratorsTuningMap, sensamagic, "minimax-copfr-S", optimizeGeneratorsTuningMap[sensamagic, "primes minimax-U"]];




(* ___ PRIVATE ___ *)

(* getPrimeCentsMap *)
test[getPrimeCentsMap, {{{12, 19, 28}}, "co", {2, 3, 5}}, {{{1200 * Log2[2], 1200 * Log2[3], 1200 * Log2[5]}}, "co"}];
test[getPrimeCentsMap, {{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "co", {2, 9, 5, 21}}, {{{1200 * Log2[2], 1200 * Log2[9], 1200 * Log2[5], 1200 * Log2[21]}}, "co"}];

(* getOddDiamond *)
test[getOddDiamond, 2, {{{2, -1}, {-1, 1}}, "contra"}];
test[getOddDiamond, 3, {{{2, -1, 0}, {3, 0, -1}, {-1, 1, 0}, {1, 1, -1}, {-2, 0, 1}, {0, -1, 1}}, "contra"}];
test[getOddDiamond, 4, {{{2, -1, 0, 0}, {3, 0, -1, 0}, {3, 0, 0, -1}, {4, -2, 0, 0}, {-1, 1, 0, 0}, {1, 1, -1, 0}, {2, 1, 0, -1}, {-2, 0, 1, 0}, {0, -1, 1, 0}, {1, 0, 1, -1}, {1, -2, 1, 0}, {-2, 0, 0, 1}, {-1, -1, 0, 1}, {0, 0, -1, 1}, {1, -2, 0, 1}, {-3, 2, 0, 0}, {0, 2, -1, 0}, {0, 2, 0, -1}}, "contra"}];

(* octaveReduce *)
test[octaveReduce, 3, 3 / 2];
test[octaveReduce, 5, 5 / 4];
test[octaveReduce, 2 / 3, 4 / 3];

(* oddLimitFromD *)
test[oddLimitFromD, 2, 3];
test[oddLimitFromD, 3, 5];
test[oddLimitFromD, 4, 9];
test[oddLimitFromD, 5, 11];
test[oddLimitFromD, 6, 15];

(* getComplexity *)
dummy5limitTemp = {{{1, 2, 3}, {0, 5, 6}}, "co"};
test[getComplexity, {1, 1, -1}, dummy5limitTemp, 1, True, 0, 0, False, 3];
test[getComplexity, {1, 1, -1}, dummy5limitTemp, 2, True, 0, 0, False, \[Sqrt]3];
test[getComplexity, {1, 1, -1}, dummy5limitTemp, 1, False, 0, 0, False, 1 +FractionBox[RowBox[{"Log", "[", "3", "]"}], RowBox[{"Log", "[", "2", "]"}]]+FractionBox[RowBox[{"Log", "[", "5", "]"}], RowBox[{"Log", "[", "2", "]"}]]];

pcv = {1, -2, 1};
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "copfr", 1, 4];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "copfr", 2, 2.449];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logProduct", 1, 6.492];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logProduct", 2, 4.055];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logIntegerLimit", 1, 3.322];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logIntegerLimit", 2, 2.029];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logOddLimit", 1, 3.170];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logOddLimit", 2, 2.010];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "product", 1, 13];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "product", 2, 8.062];

testCloseNotList[getComplexity, pcv, dummy5limitTemp, "copfr", 1, getPcvCopfrComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logProduct", 1, getPcvLogProductComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logIntegerLimit", 1, getPcvLogIntegerLimitComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logOddLimit", 1, getPcvLogOddLimitComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "product", 1, getPcvProductComplexity[pcv]];

(* getGeneratorsTuningMapMeanDamage *)
testCloseNotList[getGeneratorsTuningMapMeanDamage, meantone, "⟨1201.70 697.564]", "minimax-S", 1.700];
testCloseNotList[getGeneratorsTuningMapMeanDamage, meantone, "⟨1199.02 695.601]", "odd-diamond minisos-U", 4.186];
testCloseNotList[getGeneratorsTuningMapMeanDamage, meantone, "⟨1200.00 696.578]", "odd-diamond minimax-U", 5.377];

(* getTuningMapMeanDamage *)
testCloseNotList[getTuningMapMeanDamage, meantone, "⟨1200.000 1897.564 2786.314]", {"targetedIntervals" -> "{2,3,5}", "damageWeightingSlope" -> "unweighted", "optimizationPower" -> \[Infinity]}, 4.391];
testCloseNotList[getTuningMapMeanDamage, "⟨12 29 28]", "⟨1200 1900 2800]", fiveOddLimitDiamond <> " minisos-U", 12.052];
testCloseNotList[getTuningMapMeanDamage, "⟨12 29 28]", "⟨1200 1900 2800]", fiveOddLimitDiamond <> " minisum-U", 10.428];

(* getGeneratorsTuningMapDamages *)
testCloseNoParse[getGeneratorsTuningMapDamages, meantone, "⟨1201.7 697.564]", "minimax-S", {2 -> 1.700, 3 -> 1.6978, 5 -> 1.6978}];
testCloseNoParse[getGeneratorsTuningMapDamages, meantone, "⟨1199.02 695.601]", "odd-diamond minisos-U", {FractionBox["4", "3"] -> 5.374, FractionBox["8", "5"] -> 0.9697, FractionBox["3", "2"] -> 6.354, FractionBox["6", "5"] -> 4.4043, FractionBox["5", "4"] -> 1.9497, FractionBox["5", "3"] -> 3.4243}];
testCloseNoParse[getGeneratorsTuningMapDamages, meantone, "⟨1200.0 696.578]", "odd-diamond minimax-U", {FractionBox["4", "3"] -> 5.377, FractionBox["8", "5"] -> 0.0017, FractionBox["3", "2"] -> 5.377, FractionBox["6", "5"] -> 5.3753, FractionBox["5", "4"] -> 0.0017, FractionBox["5", "3"] -> 5.3753}];

(* getTuningMapDamages *)
testCloseNoParse[getTuningMapDamages, meantone, "⟨1200.000 1897.564 2786.314]", {"targetedIntervals" -> "{2,3,5}", "damageWeightingSlope" -> "unweighted", "optimizationPower" -> \[Infinity]}, {2 -> 0.000, 3 -> 4.391, 5 -> 0.00029}];
testCloseNoParse[getTuningMapDamages, "⟨12 29 28]", "⟨1200 1900 2800]", fiveOddLimitDiamond <> " minisos-U", {FractionBox["3", "2"] -> 1.955, FractionBox["4", "3"] -> 1.955, FractionBox["5", "4"] -> 13.68628, FractionBox["8", "5"] -> 13.68628, FractionBox["5", "3"] -> 15.6413, FractionBox["6", "5"] -> 15.6413}];
testCloseNoParse[getTuningMapDamages, "⟨12 29 28]", "⟨1200 1900 2800]", fiveOddLimitDiamond <> " minisum-U", {FractionBox["3", "2"] -> 1.955, FractionBox["4", "3"] -> 1.955, FractionBox["5", "4"] -> 13.68628, FractionBox["8", "5"] -> 13.68628, FractionBox["5", "3"] -> 15.6413, FractionBox["6", "5"] -> 15.6413}];

(* tuningInverse *)
test[tuningInverse, {{{Log2[2], 0, 0}, {0, Log2[3], 0}, {0, 0, Log2[5]}}, "co"}, {{{1 / Log2[2], 0, 0}, {0, 1 / Log2[3], 0}, {0, 0, 1 / Log2[5]}}, "co"}];
test[tuningInverse, {{{Log2[2], 0, 0}, {0, Log2[3], 0}, {0, 0, Log2[5]}, {Log2[2], Log2[3], Log[5]}}, "co"}, {{{1 / Log2[2], 0, 0, 0}, {0, 1 / Log2[3], 0, 0}, {0, 0, 1 / Log2[5], 0}}, "co"}];

(* getDualPower *)
test[getDualPower, 1, \[Infinity]];
test[getDualPower, 2, 2];
test[getDualPower, \[Infinity], 1];

(*
sources:
[1] Facebook https://www.facebook.com
[1a] https://www.facebook.com/groups/xenharmonicmath/posts/2363908480416027/?comment_id=2363994823740726
[1b] https://www.facebook.com/groups/xenharmonicmath/posts/2086012064872338/
[1c] https://www.facebook.com/groups/xenharmonicmath/posts/1035558283251060/?comment_id=1041634519310103&reply_comment_id=1041649585975263
[1d] https://www.facebook.com/groups/xenharmonicmath/posts/478197012320526/?comment_id=478441632296064
[1e] https://www.facebook.com/groups/xenharmonicmath/posts/738498989623659/?comment_id=738515309622027
[1f] (link lost, sorry) "The POTOP generators for Septimal Meantone and 5-limit meantone, meanwhile, are identical at about 696.58 cents."
[2] Yahoo posts https://yahootuninggroupsultimatebackup.github.io
[2a] https://yahootuninggroupsultimatebackup.github.io/tuning-math/topicId_21029
[2b] https://yahootuninggroupsultimatebackup.github.io/tuning-math/topicId_15819
[3] Graham's temperament app http://x31eq.com/temper/
[3a] http://x31eq.com/cgi-bin/rt.cgi?ets=3_7&limit=5
[3b] http://x31eq.com/cgi-bin/rt.cgi?ets=12_3&limit=5
[3c] http://x31eq.com/cgi-bin/rt.cgi?ets=7_2p&limit=5
[3d] http://x31eq.com/cgi-bin/rt.cgi?ets=7_15&limit=5
[3e] http://x31eq.com/cgi-bin/rt.cgi?ets=12_34&limit=5
[3f] http://x31eq.com/cgi-bin/rt.cgi?ets=53_19&limit=5
[3g] http://x31eq.com/cgi-bin/rt.cgi?ets=19_22&limit=5
[3h] http://x31eq.com/cgi-bin/rt.cgi?ets=19_10&limit=5
[3i] http://x31eq.com/cgi-bin/rt.cgi?ets=7_34&limit=5
[3j] http://x31eq.com/cgi-bin/rt.cgi?ets=12_19&limit=7
[3k] http://x31eq.com/cgi-bin/rt.cgi?ets=19_22&limit=7
[3l] http://x31eq.com/cgi-bin/rt.cgi?ets=12_10&limit=7
[3m] http://x31eq.com/cgi-bin/rt.cgi?ets=12_15&limit=7
[3n] http://x31eq.com/cgi-bin/rt.cgi?ets=19_27&limit=7
[3o] http://x31eq.com/cgi-bin/rt.cgi?ets=27_19_22&limit=7
[3p] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=3_7&tuning=po
[3q] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=12_3&tuning=po
[3r] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_2p&tuning=po
[3s] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_15&tuning=po
[3t] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=12_34&tuning=po
[3u] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=53_19&tuning=po
[3v] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=19_22&tuning=po
[3w] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=19_10&tuning=po
[3x] http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_34&tuning=po
[3y] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_19&tuning=po
[3z] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=19_22&tuning=po
[3aa] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_10&tuning=po
[3ab] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_15&tuning=po
[3ac] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=19_27&tuning=po
[3ad] http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=27_19_22&tuning=po
[4] Flora's temperament app https://github.com/FloraCanou/te_temperament_measures
[5] Paul's papers 
[5a] 
[6] Graham's papers http://x31eq.com/tuning.htm
[6a] 
[7] Xen wiki https://en.xen.wiki
[7a] https://en.xen.wiki/w/Target_tunings#Example
[7b] https://en.xen.wiki/w/Augene
[7c] https://en.xen.wiki/w/Porcupine
[7d] https://en.xen.wiki/w/Magic
[7e] https://en.xen.wiki/w/Tetracot_family#Tetracot
[7f] https://en.xen.wiki/w/Meantone
[7g] https://en.xen.wiki/w/Sensipent_family#Septimal_sensi
[7h] https://en.xen.wiki/w/Sensamagic_family#Sensamagic
[7i] https://en.xen.wiki/w/Myna#Tuning_spectrum
[7j] https://en.xen.wiki/w/Pajara#Tuning_spectrum
[7k] https://en.xen.wiki/w/Chromatic_pairs#Voltage
[8] Sintel's app https://github.com/Sin-tel/temper
[8a] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=81%2F80&submit_comma=submit
[8b] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=256%2F243&submit_comma=submit
[8c] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=25%2F24&submit_comma=submit
[8d] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=128%2F125&submit_comma=submit
[8e] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=135%2F128&submit_comma=submit
[8f] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=250%2F243&submit_comma=submit
[8g] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=2048%2F2025&submit_comma=submit
[8h] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=15625%2F15552&submit_comma=submit
[8i] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=3125%2F3072&submit_comma=submit
[8j] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=16875%2F16384&submit_comma=submit
[8k] https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=20000%2F19683&submit_comma=submit
[8l] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=81%2F80%2C+126%2F125&submit_comma=submit
[8m] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=225%2F224%2C+245%2F243&submit_comma=submit
[8n] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=50%2F49%2C+64%2F63&submit_comma=submit
[8o] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=64%2F63%2C+126%2F125&submit_comma=submit
[8p] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=126%2F125%2C+245%2F243&submit_comma=submit
[8q] https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=245%2F243&submit_comma=submit
[9] Scala
[10] Discord history https://discord.com/channels/332357996569034752
[10a] https://discord.com/channels/332357996569034752/859884647337033738/969259730839171123
[11] Keenan Pepper's tiptop.py https://github.com/YahooTuningGroupsUltimateBackup/YahooTuningGroupsUltimateBackup/blob/master/src/tuning-math/files/KeenanPepper/tiptop.py
[12] Mike Battaglia's tipweil.py variation on tiptop.py https://github.com/YahooTuningGroupsUltimateBackup/YahooTuningGroupsUltimateBackup/blob/master/src/tuning-math/files/MikeBattaglia/tipweil.py
*)


printWrapper["TOTAL FAILURES: ", failures];
printWrapper["TOTAL PASSES: ", passes];


format = "EBK";
optimizeGeneratorsTuningMap[meantone, {"systematicTuningSchemeName" -> "odd-diamond minimax-U", "logging" -> True}]