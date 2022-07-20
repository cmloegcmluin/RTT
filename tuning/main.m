(*
  
  optimizeGeneratorsTuningMap[t, tuningScheme]
  
  Given a representation of a temperament as a mapping or comma basis,
  and a tuning scheme, returns the optimum generator tuning map.
  
  The tuning scheme may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeGeneratorsTuningMap[meantoneM, {"optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"}]
    
  Out   "⟨1201.69 697.563]"
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeGeneratorsTuningMap[meantoneM, "TOP"]
    
  Out   "⟨1201.70 697.563]"
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeGeneratorsTuningMap[meantoneM, "minisos-copfr-EC"]
    
  Out   "⟨1198.24 695.294]"
  
*)
optimizeGeneratorsTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeGeneratorsTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];
optimizeGeneratorsTuningMapPrivate[t_, tuningSchemeSpec_] := Module[
  {
    tuningSchemeOptions,
    forDamage,
    tuningSchemeProperties,
    optimumGeneratorsTuningMap,
    tPossiblyWithChangedIntervalBasis,
    targetedIntervalsA,
    unchangedIntervalsA,
    complexitySizeFactor,
    tuningSchemeIntervalBasis,
    pureStretchedIntervalV,
    logging,
    approximationParts,
    powerPart,
    solution
  },
  
  forDamage = False;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  tPossiblyWithChangedIntervalBasis = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetedIntervalsA = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervalsA"]; (* trait 0a *)
  unchangedIntervalsA = tuningSchemeProperty[tuningSchemeProperties, "unchangedIntervalsA"]; (* trait 0b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 4c *)
  tuningSchemeIntervalBasis = tuningSchemeProperty[tuningSchemeProperties, "tuningSchemeIntervalBasis"]; (* trait 8 *)
  pureStretchedIntervalV = tuningSchemeProperty[tuningSchemeProperties, "pureStretchedIntervalV"]; (* trait 9 *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  approximationParts = If[
    Length[targetedIntervalsA] == 0,
    getInfiniteTargetSetTuningSchemeApproximationParts[tuningSchemeProperties],
    getApproximationParts[tuningSchemeProperties]
  ];
  
  powerPart = approximationPart[approximationParts, "powerPart"];
  
  solution = If[
    Length[unchangedIntervalsA] > 0,
    
    (* covers minimax-lol-ES "KE", unchanged-octave minimax-ES "CTE" *)
    If[logging == True, printWrapper["power solver"]];
    powerSumSolution[approximationParts, unchangedIntervalsA],
    
    If[
      powerPart == 2,
      
      (* covers odd-diamond minisos-U "least squares", 
      minimax-ES "TE", minimax-copfr-ES "Frobenius", pure-stretched-octave minimax-ES "POTE", 
      minimax-lil-ES "WE", minimax-sopfr-ES "BE" *)
      If[logging == True, printWrapper["pseudoinverse"]];
      pseudoinverseSolution[approximationParts, unchangedIntervalsA],
      
      If[
        powerPart == \[Infinity],
        
        (* covers odd-diamond minimax-U "minimax", 
        minimax-S "TOP", pure-stretched-octave minimax-S "POTOP", 
        minimax-sopfr-S "BOP", minimax-lil-S "Weil", minimax-lol-S "Kees" *)
        If[logging == True, printWrapper["max polytope"]];
        maxPolytopeSolution[approximationParts, unchangedIntervalsA],
        
        If[
          powerPart == 1,
          
          (* no historically described tuning schemes use this *)
          If[logging == True, printWrapper["sum polytope"]];
          sumPolytopeSolution[approximationParts, unchangedIntervalsA],
          
          (* no historically described tuning schemes go here *)
          If[logging == True, printWrapper["power solver"]];
          powerSumSolution[approximationParts, unchangedIntervalsA]
        ]
      ]
    ]
  ];
  
  If[
    solution == Null,
    If[logging == True, printWrapper["power limit solver"]];
    solution = powerSumLimitSolution[approximationParts, unchangedIntervalsA]
  ];
  
  optimumGeneratorsTuningMap = solution;
  
  If[
    Length[getA[targetedIntervalsA]] == 0 && complexitySizeFactor != 0,
    optimumGeneratorsTuningMap = {Drop[getA[optimumGeneratorsTuningMap], -1], "co"}
  ];
  
  If[
    !isStandardPrimeLimitIntervalBasis[getIntervalBasis[t]] && tuningSchemeIntervalBasis == "primes",
    optimumGeneratorsTuningMap = retrievePrimesIntervalBasisGeneratorsTuningMap[optimumGeneratorsTuningMap, t, tPossiblyWithChangedIntervalBasis]
  ];
  
  If[
    ToString[pureStretchedIntervalV] != "Null",
    optimumGeneratorsTuningMap = getPureStretchedIntervalGeneratorsTuningMap[optimumGeneratorsTuningMap, t, pureStretchedIntervalV]
  ];
  
  (* Print["ehehehehehe", optimumGeneratorsTuningMap];*)
  
  joe = SetAccuracy[N[optimumGeneratorsTuningMap], outputPrecision];
  
  (*Print["joe: ", joe];*)
  
  joe
];


(*
  
  optimizeTuningMap[t, tuningScheme]
  
  Given a representation of a temperament as a mapping or comma basis,
  and a tuning scheme, returns the optimum tuning map.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeTuningMap[meantoneM, {"optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"}]
    
  Out   "⟨1201.69 1899.26 2790.25]"
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeTuningMap[meantoneM, "TOP"]
    
  Out   "⟨1201.70 1899.26 2790.25]"
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        optimizeTuningMap[meantoneM, "minisos-copfr-EC"]
    
  Out   "⟨1198.24 1893.54 2781.18]" 
  
*)
optimizeTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];
optimizeTuningMapPrivate[t_, tuningSchemeSpec_] := optimizeGeneratorsTuningMapPrivate[t, tuningSchemeSpec].getA[getM[t]];

(*
  
  getGeneratorsTuningMapMeanDamage[t, generatorsTuningMap, tuningScheme]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map for that temperament, and a tuning scheme, 
  returns how much damage this tuning map causes this temperament using this tuning scheme.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaGeneratorsTuningMap = "⟨1200.000 696.578]";
        getGeneratorsTuningMapMeanDamage[meantoneM, quarterCommaGeneratorsTuningMap, "minimax-S"]
    
  Out   3.39251
  
*)
getGeneratorsTuningMapMeanDamage[unparsedT_, unparsedGeneratorsTuningMap_, tuningSchemeSpec_] := getGeneratorsTuningMapMeanDamagePrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedGeneratorsTuningMap], tuningSchemeSpec];
getGeneratorsTuningMapMeanDamagePrivate[t_, generatorsTuningMap_, tuningSchemeSpec_] := Module[
  {tuningMap},
  
  tuningMap = {{First[getA[generatorsTuningMap]].getA[getM[t]]}, "co"};
  
  getTuningMapMeanDamagePrivate[t, tuningMap, tuningSchemeSpec]
];

(*
  
  getTuningMapMeanDamage[t, tuningMap, tuningScheme]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map for that temperament, and a tuning scheme, 
  returns how much damage this tuning map causes this temperament using this tuning scheme.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaTuningMap = "⟨1200.000 1896.578 2786.314]";
        getTuningMapMeanDamage[meantoneM, quarterCommaTuningMap, "minimax-S"]
    
  Out   3.39236
  
*)
getTuningMapMeanDamage[unparsedT_, unparsedTuningMap_, tuningSchemeSpec_] := getTuningMapMeanDamagePrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap], tuningSchemeSpec];
getTuningMapMeanDamagePrivate[t_, tuningMap_, tuningSchemeSpec_] := Module[
  {
    forDamage,
    tuningSchemeOptions,
    tuningSchemeProperties,
    optimizationPower,
    targetedIntervalsA,
    approximationParts
  },
  
  forDamage = True;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"];
  targetedIntervalsA = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervalsA"]; (* trait 0a *)
  
  approximationParts = If[
    Length[targetedIntervalsA] == 0,
    getInfiniteTargetSetTuningSchemeApproximationParts[tuningSchemeProperties],
    getApproximationParts[tuningSchemeProperties]
  ];
  (* set the temperedSideGeneratorsPart to the input tuningMap, in octaves, in the structure getAbsErrors needs it, 
  since getPowerMeanAbsError shares it with other methods *)
  approximationParts[[1]] = getA[tuningMap];
  (* override the other half of the temperedSideMappingPart too, since we have the whole tuning map already *)
  approximationParts[[2]] = IdentityMatrix[getDPrivate[t]];
  
  SetAccuracy[N[getPowerMeanAbsError[approximationParts]], outputPrecision]
];

(*
  
  getGeneratorsTuningMapDamages[t, generatorsTuningMap, tuningScheme]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map for that temperament, and a tuning scheme, 
  returns the damages to each of the targeted intervals.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaGeneratorsTuningMap = "⟨1200.000 696.578]";
        getGeneratorsTuningMapDamages[meantoneM, quarterCommaGeneratorsTuningMap, "minimax-S"]
    
  Out   {2 -> 0.000, 3 -> 3.393, 5 -> 0.000}
  
*)
getGeneratorsTuningMapDamages[unparsedT_, unparsedGeneratorsTuningMap_, tuningSchemeSpec_] := getGeneratorsTuningMapDamagesPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedGeneratorsTuningMap], tuningSchemeSpec];
getGeneratorsTuningMapDamagesPrivate[t_, generatorsTuningMap_, tuningSchemeSpec_] := Module[
  {tuningMap},
  
  tuningMap = {{First[getA[generatorsTuningMap]].getA[getM[t]]}, "co"};
  
  getTuningMapDamagesPrivate[t, tuningMap, tuningSchemeSpec]
];

(*
  
  getTuningMapDamages[t, tuningMap, tuningScheme]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map for that temperament, and a tuning scheme, 
  returns the damages to each of the targeted intervals.
  
  The tuning scheme may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaTuningMap = "⟨1200.000 1896.578 2786.314]";
        getTuningMapDamages[meantoneM, quarterCommaTuningMap, "minimax-S"]
    
  Out   {2 -> 0.000, 3 -> 3.393, 5 -> 0.000}
  
*)
getTuningMapDamages[unparsedT_, unparsedTuningMap_, tuningSchemeSpec_] := getTuningMapDamagesPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap], tuningSchemeSpec];
getTuningMapDamagesPrivate[t_, tuningMap_, tuningSchemeSpec_] := Module[
  {
    forDamage,
    tuningSchemeOptions,
    tuningSchemeProperties,
    optimizationPower,
    targetedIntervalsA,
    approximationParts,
    damages,
    targetedIntervals
  },
  
  forDamage = True;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"];
  targetedIntervalsA = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervalsA"]; (* trait 0a *)
  
  approximationParts = If[
    Length[targetedIntervalsA] == 0,
    getInfiniteTargetSetTuningSchemeApproximationParts[tuningSchemeProperties],
    getApproximationParts[tuningSchemeProperties]
  ];
  (* set the temperedSideGeneratorsPart to the input tuningMap, in octaves, in the structure getAbsErrors needs it, 
  since getPowerMeanAbsError shares it with other methods *)
  approximationParts[[1]] = getA[tuningMap];
  (* override the other half of the temperedSideMappingPart too, since we have the whole tuning map already *)
  approximationParts[[2]] = IdentityMatrix[getDPrivate[t]];
  
  damages = SetAccuracy[N[getAbsErrors[approximationParts]], outputPrecision];
  targetedIntervals = Map[pcvToQuotient, targetedIntervalsA];
  
  MapThread[#1 -> #2&, {targetedIntervals, damages}]
];

(*
  
  graphTuningDamage[t, tuningScheme]
  
  Given a representation of a temperament as a mapping or comma basis, and a tuning scheme,
  graphs the damage to the targeted intervals within a close range around the optimum tuning.
  Graphs in 2D for a rank-1 temperament, 3D for a rank-2 temperament, and errors otherwise.
  
  The tuning scheme may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        graphTuningDamage[meantoneM, "minisos-copfr-EC"]
    
  Out   (3D graph)
  
  In    12etM = "⟨12 19 28]";
        graphTuningDamage[12etM, "minisos-copfr-EC"]
        
  Out   (2D graph)
  
*)
graphTuningDamage[unparsedT_, tuningSchemeSpec_] := Module[
  {
    t,
    
    forDamage,
    
    tuningSchemeOptions,
    optimumGeneratorsTuningMap,
    
    tuningSchemeProperties,
    
    optimizationPower,
    damageWeightingSlope,
    complexityNormPower,
    complexityNegateLogPrimeCoordination,
    complexityPrimePower,
    complexitySizeFactor,
    complexityMakeOdd,
    
    tWithPossiblyChangedIntervalBasis,
    targetedIntervalsA,
    
    generatorsTuningMap,
    m,
    primeCentsMap,
    
    normPower,
    plotArgs,
    targetedIntervalGraphs,
    r,
    plotStyle,
    image
  },
  
  t = parseTemperamentData[unparsedT];
  
  forDamage = True;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  optimumGeneratorsTuningMap = optimizeGeneratorsTuningMapPrivate[t, tuningSchemeOptions];
  
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  tWithPossiblyChangedIntervalBasis = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetedIntervalsA = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervalsA"]; (* trait 0a *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = tuningSchemeProperty[tuningSchemeProperties, "damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = tuningSchemeProperty[tuningSchemeProperties, "complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = tuningSchemeProperty[tuningSchemeProperties, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = tuningSchemeProperty[tuningSchemeProperties, "complexityMakeOdd"]; (* trait 4d *)
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  plotArgs = {};
  
  (* data *)
  targetedIntervalGraphs = Map[
    Function[
      {targetedIntervalPcv},
      
      Abs[multiply[{generatorsTuningMap, m, targetedIntervalPcv}, "co"] - multiply[{primeCentsMap, targetedIntervalPcv}, "co"]] / getComplexity[
        targetedIntervalPcv,
        tWithPossiblyChangedIntervalBasis,
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordination, (* trait 4a *)
        complexityPrimePower, (* trait 4b *)
        complexitySizeFactor, (* trait 4c *)
        complexityMakeOdd (* trait 4d *)
      ]
    ],
    targetedIntervalsA
  ];
  
  normPower = If[
    optimizationPower == \[Infinity] && damageWeightingSlope == "simplicityWeighted" && Length[targetedIntervals] == 0,
    getDualPower[complexityNormPower],
    optimizationPower
  ];
  AppendTo[plotArgs, {targetedIntervalGraphs, Norm[targetedIntervalGraphs, normPower]}];
  
  image = Image[
    Map[
      Map[
        If[
          # == 1,
          {0, 0, 0, 1},
          {0, 0, 0, 0}
        ]&,
        #
      ]&,
      Array[(-1)^+ ## &, {32, 32}]
    ],
    ColorSpace -> "RGB"
  ];
  image = ImageResize[image, 256, Resampling -> "Constant"];
  
  plotStyle = Join[Table[Auto, Length[targetedIntervalGraphs]], {If[r == 1, {Black, Dashed}, {Texture[image]}]}];
  
  If[debug == True, printWrapper[plotStyle]];
  
  (* range *)
  MapIndexed[AppendTo[plotArgs, {Part[generatorsTuningMap, First[#2]], #1 - 2, #1 + 2}]&, optimumGeneratorsTuningMap];
  
  (* settings *)
  AppendTo[plotArgs, ImageSize -> 1000];
  AppendTo[plotArgs, PlotStyle -> plotStyle];
  AppendTo[plotArgs, MaxRecursion -> 6];
  
  (* plot type *)
  r = getRPrivate[tWithPossiblyChangedIntervalBasis];
  If[
    r == 1,
    Apply[Plot, plotArgs],
    If[
      r == 2,
      Apply[Plot3D, plotArgs],
      Throw["4D and higher visualizations not supported"]
    ]
  ]
];

(*
  
  generatorsTuningMapFromTAndTuningMap[t, tuningMap]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map, returns the generators tuning map.
  
  Examples:
  
  In    meantoneM = "[⟨1 1 0] ⟨0 1 4]⟩";
        quarterCommaTuningMap = "⟨1200.000 1896.578 2786.314]";
        generatorsTuningMapFromTAndTuningMap[meantoneM, quarterCommaTuningMap]
    
  Out   "⟨1200.000 696.578]";
  
*)
generatorsTuningMapFromTAndTuningMap[unparsedT_, unparsedTuningMap_] := formatOutput[generatorsTuningMapFromTAndTuningMapPrivate[parseTemperamentData[unparsedT], parseTemperamentData[unparsedTuningMap]]];
generatorsTuningMapFromTAndTuningMapPrivate[t_, tuningMap_] := Module[
  {generatorsTuningMap, m, primeCentsMap, solution},
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  solution = NMinimize[Norm[First[getA[multiply[{generatorsTuningMap, m}, "co"]]] - First[getA[tuningMap]]], generatorsTuningMap];
  
  {{generatorsTuningMap /. Last[solution]}, "co"}
];




(* ___ PRIVATE ___ *)



(* TUNING SCHEME OPTIONS *)

linearSolvePrecision = 8;
nMinimizePrecision = 128;
absoluteValuePrecision = nMinimizePrecision * 2;

processTuningSchemeSpec[tuningSchemeSpec_] := If[
  StringQ[tuningSchemeSpec],
  If[
    StringMatchQ[tuningSchemeSpec, RegularExpression["(?:.* )?mini(?:max|sos|sum)-(?:\\w+-)?E?[UCS]"]],
    {"systematicTuningSchemeName" -> tuningSchemeSpec},
    {"originalTuningSchemeName" -> tuningSchemeSpec}
  ],
  tuningSchemeSpec
];

tuningSchemeOptions = {
  "targetedIntervals" -> Null, (* trait 0a *)
  "unchangedIntervals" -> {}, (* trait 0b *)
  "optimizationPower" -> Null, (* trait 1: \[Infinity] = minimax, 2 = minisos, 1 = minisum *)
  "damageWeightingSlope" -> "", (* trait 2: unweighted, complexityWeighted, or simplicityWeighted *)
  "complexityNormPower" -> 1, (* trait 3: what Mike Battaglia refers to as `p` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space *)
  "complexityNegateLogPrimeCoordination" -> False, (* trait 4a: False = do nothing, True = negate the multiplication by logs of primes *)
  "complexityPrimePower" -> 0, (* trait 4b: what Mike Battaglia refers to as `s` in https://en.xen.wiki/w/BOP_tuning; 0 = nothing, equiv to copfr when log prime coordination is negated and otherwise defaults; 1 = product complexity, equiv to sopfr when log prime coordination is negated and otherwise defaults; >1 = pth power of those *)
  "complexitySizeFactor" -> 0, (* trait 4c: what Mike Battaglia refers to as `k` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space; 0 = no augmentation to factor in span, 1 = could be integer limit, etc. *)
  "complexityMakeOdd" -> False, (* trait 4d: False = do nothing, True = achieve odd limit from integer limit, etc. *)
  "tuningSchemeIntervalBasis" -> "primes", (* trait 8: Graham Breed calls this "inharmonic" vs "subgroup" notion in the context of minimax-ES ("TE") tuning, but it can be used for any tuning *)
  "pureStretchedInterval" -> Null, (* trait 9 *)
  "systematicTuningSchemeName" -> "",
  "originalTuningSchemeName" -> "",
  "systematicDamageName" -> "",
  "originalDamageName" -> "",
  "systematicComplexityName" -> "",
  "originalComplexityName" -> "",
  "logging" -> False
};
Options[processTuningSchemeOptions] = tuningSchemeOptions;
processTuningSchemeOptions[t_, forDamage_, OptionsPattern[]] := Module[
  {
    targetedIntervals, (* trait 0a *)
    unchangedIntervals, (* trait 0b *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningSchemeIntervalBasis, (* trait 8 *)
    pureStretchedInterval, (* trait 9 *)
    systematicTuningSchemeName,
    originalTuningSchemeName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    logging,
    tPossiblyWithChangedIntervalBasis,
    targetedIntervalsA,
    unchangedIntervalsA,
    pureStretchedIntervalV,
    commaBasisInNonstandardIntervalBasis,
    primeLimitIntervalBasis,
    commaBasisInPrimeLimitIntervalBasis,
    mappingInPrimeLimitIntervalBasis,
    intervalBasis,
    intervalRebase
  },
  
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0a *)
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 0b *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningSchemeIntervalBasis = OptionValue["tuningSchemeIntervalBasis"]; (* trait 8 *)
  pureStretchedInterval = OptionValue["pureStretchedInterval"]; (* trait 9 *)
  systematicTuningSchemeName = OptionValue["systematicTuningSchemeName"];
  originalTuningSchemeName = OptionValue["originalTuningSchemeName"];
  systematicDamageName = OptionValue["systematicDamageName"];
  originalDamageName = OptionValue["originalDamageName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  logging = OptionValue["logging"];
  
  If[
    originalTuningSchemeName === "minimax",
    optimizationPower = \[Infinity]; damageWeightingSlope = "unweighted"; unchangedIntervals = "octave";
  ];
  If[
    originalTuningSchemeName === "least squares",
    optimizationPower = 2; damageWeightingSlope = "unweighted"; unchangedIntervals = "octave";
  ];
  If[
    originalTuningSchemeName === "TOP" || originalTuningSchemeName === "TIPTOP" || originalTuningSchemeName === "T1" || originalTuningSchemeName === "TOP-max",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    originalTuningSchemeName === "TE" || originalTuningSchemeName === "Tenney-Euclidean" || originalTuningSchemeName === "T2" || originalTuningSchemeName === "TOP-RMS",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "E";
  ];
  If[
    originalTuningSchemeName === "Frobenius",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "copfr-E";
  ];
  If[
    originalTuningSchemeName === "BOP",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "sopfr";
  ];
  If[
    originalTuningSchemeName === "BE" || originalTuningSchemeName === "Benedetti-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  systematicComplexityName = "sopfr-E";
  ];
  If[
    originalTuningSchemeName === "Weil",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";systematicComplexityName = "lil";
  ];
  If[
    originalTuningSchemeName === "WE" || originalTuningSchemeName === "Weil-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "lil-E";
  ];
  If[
    originalTuningSchemeName === "Kees",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  systematicComplexityName = "lol";
  ];
  If[
    originalTuningSchemeName === "KE" || originalTuningSchemeName === "Kees-Euclidean",
    (* Note how this tuning scheme works by enforcing an unchanged octave via a solver constraint, rather than through the complexity units multiplier *)
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "lol-E"; unchangedIntervals = "octave";
  ];
  If[
    originalTuningSchemeName === "POTOP" || originalTuningSchemeName === "POTT",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; pureStretchedInterval = "octave";
  ];
  If[
    originalTuningSchemeName === "POTE",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "E"; pureStretchedInterval = "octave";
  ];
  If[
    originalTuningSchemeName === "CTE" || originalTuningSchemeName === "Constrained Tenney-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "E"; unchangedIntervals = "octave";
  ];
  
  If[
    originalDamageName === "topDamage",
    damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityNegateLogPrimeCoordination = True; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  
  (* Note: we can't implement product complexity with the current design, and don't intend to revise.
   This is because product complexity is realized from a PC-vector as a product of terms,
    raised to the powers of the absolute values of the entries. But this design only multiplies entries and sums them. 
    Since sopfr achieves the same tuning, we simply treat that sopfr as the canonical approach for this effect. *)
  If[
    originalComplexityName === "copfr" || originalComplexityName === "l1Norm",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = True; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "sopfr" || originalComplexityName === "wilsonHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = True; complexityPrimePower = 1; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "integerLimit" || originalComplexityName === "weilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = True; complexityPrimePower = 0; complexitySizeFactor = 1; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "oddLimit" || originalComplexityName === "keesHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = True; complexityPrimePower = 0; complexitySizeFactor = 1; complexityMakeOdd = True;
  ];
  If[
    originalComplexityName === "logProduct" || originalComplexityName === "tenneyHeight" || originalComplexityName === "harmonicDistance",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = False; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "logIntegerLimit" || originalComplexityName === "logarithmicWeilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "logOddLimit" || originalComplexityName === "keesExpressibility",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  If[
    originalComplexityName === "rososcopfr" || originalComplexityName === "l2Norm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = True; complexitySizeFactor = 0; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "rosossopfr",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = True; complexitySizeFactor = 0; complexityPrimePower = 1; complexityMakeOdd = False;
  ];
  (* (following the pattern here, this tuning scheme might exist, but it has not been described or named) If[
    ,
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = True; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ]; *)
  (* (following the pattern here, this tuning scheme might exist, but it has not been described or named) If[
    ,
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = True; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ]; *)
  If[
    originalComplexityName === "tenneyEuclideanHeight",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = False; complexitySizeFactor = 0;  complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "weilEuclideanNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "keesEuclideanSeminorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  (* This one doesn't follow the above patterns as closely.
   See: https://www.facebook.com/groups/xenharmonicmath/posts/1426449464161938/?comment_id=1426451087495109&reply_comment_id=1426470850826466 *)
  If[
    originalComplexityName === "carlsNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = True; complexitySizeFactor = 0; complexityPrimePower = 2; complexityMakeOdd = False;
  ];
  
  (* trait 0a - targeted intervals *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*infinite-target-set*"] || (StringMatchQ[systematicTuningSchemeName, "*minimax*"] && StringMatchQ[systematicTuningSchemeName, "*S*"]),
    targetedIntervals = {};
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*odd-diamond*"],
    targetedIntervals = "odd-diamond"; unchangedIntervals = "octave";
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*primes*"],
    targetedIntervals = "primes";
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, RegularExpression["^(?:unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+)?(?:pure\\-stretched\\-\\S+\\s+)?\\{[\\d\\/\\,\\s]*\\}\\s+.*"]],
    targetedIntervals = First[StringCases[systematicTuningSchemeName, RegularExpression["^(?:unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+)?(?:pure\\-stretched\\-\\S+\\s+)?(\\{[\\d\\/\\,\\s]*\\})\\s+.*"] -> "$1"]],
  ];
  
  (* trait 0b - unchanged intervals *)
  If[
    StringMatchQ[systematicTuningSchemeName, RegularExpression["unchanged\\-\\{?[\\w\\s\\,\\/]+\\}?\\s+.*"]],
    unchangedIntervals = First[StringCases[systematicTuningSchemeName, RegularExpression["unchanged\\-(\\{?[\\w\\s\\,\\/]+\\}?)\\s+.*"] -> "$1"]];
  ];
  
  (* trait 1 - optimization power *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*minimax*"],
    optimizationPower = \[Infinity];
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*minisos*"],
    optimizationPower = 2;
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*minisum*"],
    optimizationPower = 1;
  ];
  
  (* trait 2 - damage weighting slope *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*S*"] || StringMatchQ[systematicDamageName, "*S*"],
    damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*C*"] || StringMatchQ[systematicDamageName, "*C*"],
    damageWeightingSlope = "complexityWeighted";
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*U*"] || StringMatchQ[systematicDamageName, "*U*"],
    damageWeightingSlope = "unweighted";
  ];
  
  (* trait 3 - interval complexity norm power *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*E*"] || StringMatchQ[systematicDamageName, "*E*"] || StringMatchQ[systematicComplexityName, "*E*"],
    complexityNormPower = 2;
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*T*"] || StringMatchQ[systematicDamageName, "*T*"] || StringMatchQ[systematicComplexityName, "*T*"],
    complexityNormPower = 1;
  ];
  
  (* trait 4 - interval complexity coordinate change *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*copfr*"] || StringMatchQ[systematicDamageName, "*copfr*"] || StringMatchQ[systematicComplexityName, "*copfr*"],
    complexityNegateLogPrimeCoordination = True;
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*sopfr*"] || StringMatchQ[systematicDamageName, "*sopfr*"] || StringMatchQ[systematicComplexityName, "*sopfr*"],
    complexityNegateLogPrimeCoordination = True; complexityPrimePower = 1;
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*lil*"] || StringMatchQ[systematicDamageName, "*lil*"] || StringMatchQ[systematicComplexityName, "*lil*"],
    complexitySizeFactor = 1;
  ];
  If[
    StringMatchQ[systematicTuningSchemeName, "*lol*"] || StringMatchQ[systematicDamageName, "*lol*"] || StringMatchQ[systematicComplexityName, "*lol*"],
    complexitySizeFactor = 1; complexityMakeOdd = True;
  ];
  
  (* trait 8 - tuning scheme interval basis *)
  If[
    StringMatchQ[systematicTuningSchemeName, "*formal-primes-basis*"],
    tuningSchemeIntervalBasis = "primes";
  ];
  
  (* trait 9 - pure-stretched interval *)
  If[
    StringMatchQ[systematicTuningSchemeName, RegularExpression["pure\\-stretched\\-\\S+\\s+.*"]],
    pureStretchedInterval = First[StringCases[systematicTuningSchemeName, RegularExpression["pure\\-stretched\\-(\\S+)\\s+.*"] -> "$1"]];
  ];
  
  (* complexityMakeOdd is enough to get odd limit complexity from integer limit complexity, 
  but when actually solving for tunings, it's necessary to lock down prime 2 (the octave) as an unchanged interval. *)
  If[complexityMakeOdd == True, unchangedIntervals = "octave"];
  
  (* This has to go below the systematic tuning scheme name gating, so that targetedIntervals has a change to be set to {} *)
  intervalBasis = getIntervalBasis[t];
  If[
    !isStandardPrimeLimitIntervalBasis[intervalBasis] && tuningSchemeIntervalBasis == "primes",
    
    commaBasisInNonstandardIntervalBasis = getC[t];
    primeLimitIntervalBasis = getPrimes[getIntervalBasisDimension[intervalBasis]];
    commaBasisInPrimeLimitIntervalBasis = changeIntervalBasisPrivate[commaBasisInNonstandardIntervalBasis, primeLimitIntervalBasis];
    intervalRebase = getIntervalRebaseForC[intervalBasis, primeLimitIntervalBasis];
    mappingInPrimeLimitIntervalBasis = getM[commaBasisInPrimeLimitIntervalBasis];
    tPossiblyWithChangedIntervalBasis = mappingInPrimeLimitIntervalBasis;
    targetedIntervalsA = rebase[intervalRebase, getTargetedIntervalsA[targetedIntervals, t, tPossiblyWithChangedIntervalBasis, forDamage]];
    unchangedIntervalsA = rebase[intervalRebase, getUnchangedIntervalsA[unchangedIntervals, t]];
    pureStretchedIntervalV = rebase[intervalRebase, getPureStretchedIntervalV[pureStretchedInterval, t]],
    
    tPossiblyWithChangedIntervalBasis = t;
    targetedIntervalsA = getTargetedIntervalsA[targetedIntervals, t, tPossiblyWithChangedIntervalBasis, forDamage];
    unchangedIntervalsA = getUnchangedIntervalsA[unchangedIntervals, t];
    pureStretchedIntervalV = getPureStretchedIntervalV[pureStretchedInterval, t];
  ];
  
  If[
    logging == True,
    printWrapper["tPossiblyWithChangedIntervalBasis: ", formatOutput[tPossiblyWithChangedIntervalBasis]];
    printWrapper["targetedIntervalsA: ", formatOutput[targetedIntervalsA]]; (* trait 0a *)
    printWrapper["unchangedIntervalsA: ", formatOutput[unchangedIntervalsA]]; (* trait 0b *)
    printWrapper["optimizationPower: ", formatOutput[optimizationPower]]; (* trait 1 *)
    printWrapper["damageWeightingSlope: ", formatOutput[damageWeightingSlope]]; (* trait 2 *)
    printWrapper["complexityNormPower: ", formatOutput[complexityNormPower]]; (* trait 3 *)
    printWrapper["complexityNegateLogPrimeCoordination: ", formatOutput[complexityNegateLogPrimeCoordination]]; (* trait 4a *)
    printWrapper["complexityPrimePower: ", formatOutput[complexityPrimePower]]; (* trait 4b *)
    printWrapper["complexitySizeFactor: ", formatOutput[complexitySizeFactor]]; (* trait 4c *)
    printWrapper["complexityMakeOdd: ", formatOutput[complexityMakeOdd]]; (* trait 4d *)
    printWrapper["tuningSchemeIntervalBasis: ", formatOutput[tuningSchemeIntervalBasis]]; (* trait 8 *)
    printWrapper["pureStretchedIntervalV: ", formatOutput[pureStretchedIntervalV]]; (* trait 9 *)
  ];
  
  If[
    !NumericQ[optimizationPower] && optimizationPower != \[Infinity],
    Throw["no optimization power"]
  ];
  If[
    damageWeightingSlope == "",
    Throw["no damage weighting slope"]
  ];
  If[
    Length[targetedIntervalsA] == 0 && optimizationPower != \[Infinity],
    Throw["It is not possible to optimize for minisum or minisos over all intervals, only minimax."]
  ];
  If[
    Length[targetedIntervalsA] == 0 && damageWeightingSlope != "simplicityWeighted",
    Throw["It is not possible to minimize damage over all intervals if it is not simplicity-weighted."]
  ];
  
  {
    tPossiblyWithChangedIntervalBasis,
    targetedIntervalsA, (* trait 0a *)
    unchangedIntervalsA, (* trait 0b *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningSchemeIntervalBasis, (* trait 8 *)
    pureStretchedIntervalV, (* trait 9 *)
    logging
  }
];

tuningSchemePropertiesPartsByOptionName = <|
  "t" -> 1,
  "targetedIntervalsA" -> 2, (* trait 0a *)
  "unchangedIntervalsA" -> 3, (* trait 0b *)
  "optimizationPower" -> 4, (* trait 1 *)
  "damageWeightingSlope" -> 5, (* trait 2 *)
  "complexityNormPower" -> 6, (* trait 3 *)
  "complexityNegateLogPrimeCoordination" -> 7, (* trait 4a *)
  "complexityPrimePower" -> 8, (* trait 4b *)
  "complexitySizeFactor" -> 9, (* trait 4c *)
  "complexityMakeOdd" -> 10, (* trait 4d *)
  "tuningSchemeIntervalBasis" -> 11, (* trait 8 *)
  "pureStretchedIntervalV" -> 12, (* trait 9 *)
  "logging" -> 13
|>;
tuningSchemeProperty[tuningSchemeProperties_, optionName_] := Part[tuningSchemeProperties, tuningSchemePropertiesPartsByOptionName[optionName]];

getTargetedIntervalsA[targetedIntervals_, t_, tPossiblyWithChangedIntervalBasis_, forDamage_] := {If[
  ToString[targetedIntervals] == "Null",
  Throw["no targeted intervals"],
  If[
    ToString[targetedIntervals] == "{}",
    If[
      forDamage,
      getFormalPrimesA[tPossiblyWithChangedIntervalBasis],
      {}
    ],
    If[
      ToString[targetedIntervals] == "odd-diamond",
      getOddDiamond[getDPrivate[tPossiblyWithChangedIntervalBasis]],
      If[
        ToString[targetedIntervals] == "primes",
        IdentityMatrix[getDPrivate[tPossiblyWithChangedIntervalBasis]],
        getA[parseQuotientSet[targetedIntervals, t]]
      ]
    ]
  ]
], "contra"};

getUnchangedIntervalsA[unchangedIntervals_, t_] := If[
  ToString[unchangedIntervals] == "{}",
  Null,
  If[
    ToString[unchangedIntervals] == "octave",
    getOctave[t],
    parseQuotientSet[unchangedIntervals, t]
  ]
];

getPureStretchedIntervalV[pureStretchedInterval_, t_] := If[ (* TODO: note this is almost the same as getUnchangedIntervalsA now *)
  ToString[pureStretchedInterval] == "Null",
  Null,
  If[
    ToString[pureStretchedInterval] == "octave",
    getOctave[t],
    parseQuotientSet[pureStretchedInterval, t]
  ]
];

rebase[intervalRebase_, t_] := If[t == Null, t, multiply[{intervalRebase, t}, "co"]];


(* PARTS *)

getApproximationParts[tuningSchemeProperties_] := Module[
  {
    t,
    targetedIntervalsA,
    optimizationPower,
    logging,
    
    generatorsTuningMap,
    m,
    primeCentsMap,
    
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart,
    powerPart,
    periodsPerOctavePart
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetedIntervalsA = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervalsA"]; (* trait 0a *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 1 *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  temperedSideGeneratorsPart = generatorsTuningMap;
  temperedSideMappingPart = m;
  justSideGeneratorsPart = primeCentsMap;
  justSideMappingPart = getPrimesIdentityA[t];
  eitherSideIntervalsPart = targetedIntervalsA;
  eitherSideMultiplierPart = getDamageWeights[tuningSchemeProperties];
  powerPart = optimizationPower;
  periodsPerOctavePart = getPeriodsPerOctave[t];
  
  If[
    logging == True,
    printWrapper["temperedSideGeneratorsPart: ", formatOutput[temperedSideGeneratorsPart]]; (* g *)
    printWrapper["temperedSideMappingPart: ", formatOutput[temperedSideMappingPart]]; (* M *)
    printWrapper["justSideGeneratorsPart: ", formatOutput[justSideGeneratorsPart]]; (* p *)
    printWrapper["justSideMappingPart: ", formatOutput[justSideMappingPart]]; (* I *)
    printWrapper["eitherSideIntervalsPart: ", formatOutput[eitherSideIntervalsPart]]; (* T *)
    printWrapper["eitherSideMultiplierPart: ", formatOutput[eitherSideMultiplierPart]]; (* W *)
    printWrapper["powerPart: ", powerPart];
    printWrapper["periodsPerOctavePart: ", periodsPerOctavePart];
  ];
  
  {
    temperedSideGeneratorsPart, (* g *)
    temperedSideMappingPart, (* M *)
    justSideGeneratorsPart, (* p *)
    justSideMappingPart, (* I *)
    eitherSideIntervalsPart, (* T *)
    eitherSideMultiplierPart, (* W *)
    powerPart,
    periodsPerOctavePart
  }
];

approximationPartsByName = <|
  "temperedSideGeneratorsPart" -> 1,
  "temperedSideMappingPart" -> 2,
  "justSideGeneratorsPart" -> 3,
  "justSideMappingPart" -> 4,
  "eitherSideIntervalsPart" -> 5,
  "eitherSideMultiplierPart" -> 6,
  "powerPart" -> 7,
  "periodsPerOctavePart" -> 8
|>;
approximationPart[approximationParts_, partName_] := Part[approximationParts, approximationPartsByName[partName]];


(* SHARED *)

getOctave[t_] := {{Join[{1}, Table[0, getDPrivate[t] - 1]]}, "contra"};
getSummationMap[t_] := {{Table[1, getDPrivate[t]]}, "co"};
getLogPrimeCoordinationA[t_] := {DiagonalMatrix[Log2[getIntervalBasis[t]]], "co"};
getPrimeCentsMap[t_] := {1200 * getA[multiply[{getSummationMap[t], getLogPrimeCoordinationA[t]}, "co"]], "co"};
getPrimesIdentityA[t_] := {IdentityMatrix[getDPrivate[t]], "co"};
getPeriodsPerOctave[t_] := First[First[getA[getM[t]]]];

getTuningSchemeMappings[t_] := Module[
  {generatorsTuningMap, m, primeCentsMap},
  
  generatorsTuningMap = {{Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getRPrivate[t]}]}, "co"};
  m = getM[t];
  primeCentsMap = getPrimeCentsMap[t];
  
  {generatorsTuningMap, m, primeCentsMap}
];

(* similar to pseudoinverse, but works for any tuning so far described *)
tuningInverse[damageWeightsOrComplexityMultiplier_] := {MapThread[
  Function[
    {dataRow, zerosRow},
    MapIndexed[
      Function[
        {zerosEl, index},
        zerosEl + If[
          First[index] > Length[dataRow],
          0,
          Part[dataRow, First[index]]
        ]
      ],
      zerosRow
    ]
  ],
  {
    (* note: this is pseudo not because of non-square, due to complexity size factor,
    but because of when complexity is odd and the top-left entry is a 0 so det is 0 so it's singular *)
    PseudoInverse[
      getA[damageWeightsOrComplexityMultiplier][[1 ;; Last[Dimensions[getA[damageWeightsOrComplexityMultiplier]]]]]
    ],
    Table[
      Table[
        0,
        First[Dimensions[getA[damageWeightsOrComplexityMultiplier]]]
      ],
      Last[Dimensions[getA[damageWeightsOrComplexityMultiplier]]]
    ]
  }
], "co"};


(* DAMAGE *)

(* compare with getDualMultiplier *)
getDamageWeights[tuningSchemeProperties_] := Module[
  {
    t,
    targetedIntervalsA, (* trait 0a *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    
    damageWeights
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetedIntervalsA = tuningSchemeProperty[tuningSchemeProperties, "targetedIntervalsA"]; (* trait 0a *)
  damageWeightingSlope = tuningSchemeProperty[tuningSchemeProperties, "damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = tuningSchemeProperty[tuningSchemeProperties, "complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = tuningSchemeProperty[tuningSchemeProperties, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = tuningSchemeProperty[tuningSchemeProperties, "complexityMakeOdd"]; (* trait 4d *)
  
  damageWeights = If[
    damageWeightingSlope == "unweighted",
    
    {IdentityMatrix[Length[getA[targetedIntervalsA]]], "co"},
    
    {DiagonalMatrix[Map[Function[
      {targetedIntervalPcv},
      getComplexity[
        targetedIntervalPcv,
        t,
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordination, (* trait 4a *)
        complexityPrimePower, (* trait 4b *)
        complexitySizeFactor, (* trait 4c *)
        complexityMakeOdd (* trait 4d *)
      ]
    ], getA[targetedIntervalsA]]], "co"}
  ];
  
  If[
    damageWeightingSlope == "simplicityWeighted",
    
    tuningInverse[damageWeights],
    
    damageWeights
  ]
];


(* ERROR *)

getPowerSumAbsError[approximationParts_] := If[
  approximationPart[approximationParts, "powerPart"] == \[Infinity],
  
  (* I thought it would be fine, but apparently Wolfram Language thinks the infinity-power-sum is "indeterminate" *)
  (*Print["\[Infinity] getAbsErrors[approximationParts]: ", getAbsErrors[approximationParts]];*)
  Max[First[getA[getAbsErrors[approximationParts]]]],
  
  (*Print["getAbsErrors[approximationParts]: ", getAbsErrors[approximationParts]];*)
  Total[Power[First[getA[getAbsErrors[approximationParts]]], approximationPart[approximationParts, "powerPart"]]]
];
getPowerNormAbsError[approximationParts_] := Norm[getA[getAbsErrors[approximationParts]], approximationPart[approximationParts, "powerPart"]];
getPowerMeanAbsError[approximationParts_] := Module[
  {absErrors, powerPart, targetedIntervalCount, result},
  
  absErrors = getAbsErrors[approximationParts];
  powerPart = approximationPart[approximationParts, "powerPart"];
  targetedIntervalCount = Last[Dimensions[approximationPart[approximationParts, "eitherSideIntervalsPart"]]]; (* k *)
  
  If[debug == True, printWrapper["absErrors: ", absErrors]];
  
  result = If[
    powerPart == \[Infinity],
    
    (* again, I thought it'd be fine, but Wolfram Language thinks the infinity-power-sum is "indeterminate" *)
    Max[getA[absErrors]],
    
    Power[
      Total[Power[
        getA[absErrors],
        powerPart
      ]] / targetedIntervalCount,
      1 / powerPart
    ]
  ];
  
  result
];

(* returns errors in octaves *)
getAbsErrors[{
  temperedSideGeneratorsPart_,
  temperedSideMappingPart_,
  justSideGeneratorsPart_,
  justSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_,
  powerPart_,
  periodsPerOctavePart_
}] := Module[
  {temperedSide, justSide, absErrors},
  
  temperedSide = getTemperedOrJustSide[temperedSideGeneratorsPart, temperedSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart];
  justSide = getTemperedOrJustSide[justSideGeneratorsPart, justSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart];
  
  absErrors = {{ Abs[N[
    Map[
      If[Quiet[PossibleZeroQ[#]], 0, #]&,
      First[getA[temperedSide] - getA[justSide]]
    ],
    absoluteValuePrecision
  ]]}, "co"};
  
  If[
    debug == True,
    printWrapper[formatOutput[temperedSide]];
    printWrapper[formatOutput[justSide]];
    printWrapper["absErrors: ", Map[If[Quiet[PossibleZeroQ[#]], 0, SetAccuracy[#, 4]]&, getA[absErrors]]]
  ];
  
  absErrors
];

(* COMPLEXITY *)

(* returns complexities in weighted octaves *)
getComplexity[
  pcv_,
  t_,
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {complexityMultiplierAndLogPrimeCoordinationA},
  
  complexityMultiplierAndLogPrimeCoordinationA = getComplexityMultiplierAndLogPrimeCoordinationA[
    t,
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  (*Print["only doing the first one...",complexityMultiplierAndLogPrimeCoordinationA, " and ", multiply[{complexityMultiplierAndLogPrimeCoordinationA, {{pcv}, "contra"}}, "co"]];*)
  
  Norm[getA[multiply[{complexityMultiplierAndLogPrimeCoordinationA, {{pcv}, "contra"}}, "co"]], complexityNormPower] / (1 + complexitySizeFactor)
];

(* Note that we don't actually use any of these functions directly; they're just around to test understanding *)
getPcvCopfrComplexity[pcv_, t_] := Total[Map[If[Abs[# > 0], 1, 0]&, pcv]];
(* AKA "Benedetti height" *)
getPcvProductComplexity[pcv_, t_] := Times @@ MapThread[#1^Abs[#2]&, {getIntervalBasis[t], pcv}];
(* AKA "Tenney height" *)
getPcvLogProductComplexity[pcv_, t_] := Log2[getPcvProductComplexity[pcv, t]];
(* AKA "Wilson height", can also be used to find minimax-sopfr-S ("BOP") tuning scheme *)
getPcvSopfrComplexity[pcv_, t_] := Total[MapThread[#1 * Abs[#2]&, {getIntervalBasis[t], pcv}]];
(* This apparently doesn't have a name, but can also be used to find minimax-S ("TOP") tuning scheme *)
getPcvLogSopfrComplexity[pcv_, t_] := Log2[getPcvSopfrComplexity[pcv, t]];
(* AKA "Weil height" *)
getPcvIntegerLimitComplexity[pcv_, t_] := Module[{quotient},
  quotient = pcvToQuotient[pcv];
  Max[Numerator[quotient], Denominator[quotient]]
];
(* AKA "logarithmic Weil height", used for minimax-lil-S ("Weil") tuning scheme *)
getPcvLogIntegerLimitComplexity[pcv_, t_] := Log2[getPcvIntegerLimitComplexity[pcv, t]];
(* AKA "Kees height" *)
removePowersOfTwoFromPcv[pcv_] := MapIndexed[If[First[#2] == 1, 0, #1]&, pcv];
getPcvOddLimitComplexity[pcv_, t_] := getPcvIntegerLimitComplexity[removePowersOfTwoFromPcv[pcv], t];
(* AKA "Kees expressibility", used for minimax-lol-S ("Kees") tuning scheme *)
getPcvLogOddLimitComplexity[pcv_, t_] := Log2[getPcvOddLimitComplexity[pcv, t]];

(* This is different than getDamageWeights, this is nested within it;
this is to weight the quantities of the PC-vector entries before taking their norm to get an interval complexity, 
and these complexities are then gathered for each interval and applied 
(or their reciprocals applied, in the case of simplicity-weighting) as damageWeights;
when this method is used by getDamageWeights in getApproximationParts, 
it covers any finite-target-set tuning scheme using this for its damage's complexity *)
getComplexityMultiplier[
  t_,
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[{complexityMultiplier},
  (* when used by getDualMultiplier in getInfiniteTargetSetTuningSchemeApproximationParts, covers minimax-S ("TOP") and minimax-ES ("TE") *)
  complexityMultiplier = {IdentityMatrix[getDPrivate[t]], "co"};
  
  If[
    (* when used by getDualMultiplier in getInfiniteTargetSetTuningSchemeApproximationParts, covers minimax-copfr-S (the L1 version of "Frobenius") and minimax-copfr-ES ("Frobenius") *)
    complexityNegateLogPrimeCoordination == True,
    complexityMultiplier = multiply[{complexityMultiplier, inverse[getLogPrimeCoordinationA[t]]}, "co"]
  ];
  
  If[
    (* when used by getDualMultiplier in getInfiniteTargetSetTuningSchemeApproximationParts, covers minimax-sopfr-S ("BOP") and minimax-sopfr-ES ("BE") *)
    complexityPrimePower > 0,
    complexityMultiplier = multiply[{complexityMultiplier, {DiagonalMatrix[Power[getA[getIntervalBasis[t]], complexityPrimePower]], "co"}}, "co"]
  ];
  
  If[
    (* when used by getDualMultiplier in getInfiniteTargetSetTuningSchemeApproximationParts, covers minimax-lil-S ("Weil"), minimax-lil-ES ("WE"), minimax-lol-S ("Kees"), and minimax-lol-ES ("KE")
    (yes, surprisingly, when computing minimax-lol-S and minimax-lol-ES tunings, we do not use the below, though user calls for odd-limit complexity do use it;
    the tuning calculations instead use only this size-sensitizer effect, and apply an unchanged octave constraint to achieve the oddness aspect) *)
    complexitySizeFactor > 0,
    complexityMultiplier = multiply[{{Join[getA[getPrimesIdentityA[t]], {Table[complexitySizeFactor, getDPrivate[t]]}], "co"}, complexityMultiplier}, "co"]
  ];
  
  If[
    (* When minimax-lol-S ("Kees") and minimax-lol-ES ("KE") need their dual norms, they don't use this; see note above *)
    complexityMakeOdd == True,
    complexityMultiplier = multiply[{complexityMultiplier, {DiagonalMatrix[Join[{0}, Table[1, getDPrivate[t] - 1]]], "co"}}, "co"]
  ];
  
  complexityMultiplier
];

getComplexityMultiplierAndLogPrimeCoordinationA[
  t_,
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := multiply[
  {
    getComplexityMultiplier[
      t,
      complexityNegateLogPrimeCoordination, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityMakeOdd (* trait 4d *)
    ],
    getLogPrimeCoordinationA[t]
  },
  "co"
];


(* INFINITE-TARGET-SET *)

getDualPower[power_] := If[power == 1, \[Infinity], 1 / (1 - 1 / power)];

(* compare with getDamageWeights *)
getDualMultiplier[tuningSchemeProperties_] := Module[
  {
    t,
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    
    complexityMultiplierAndLogPrimeCoordinationA
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = tuningSchemeProperty[tuningSchemeProperties, "complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = tuningSchemeProperty[tuningSchemeProperties, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 4c *)
  (* when computing tunings (as opposed to complexities directly), complexity-make-odd is handled through constraints *)
  complexityMakeOdd = False; (* trait 4d *)
  
  complexityMultiplierAndLogPrimeCoordinationA = getComplexityMultiplierAndLogPrimeCoordinationA[
    t,
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  (* always essentially simplicity weighted *)
  tuningInverse[complexityMultiplierAndLogPrimeCoordinationA]
];

(* compare with getApproximationParts *)
getInfiniteTargetSetTuningSchemeApproximationParts[tuningSchemeProperties_] := Module[
  {
    t,
    complexityNormPower,
    complexitySizeFactor,
    logging,
    
    generatorsTuningMap,
    m,
    primeCentsMap,
    
    dualMultiplier,
    primesErrorMagnitudeNormPower,
    
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart,
    powerPart,
    periodsPerOctavePart
  },
  
  t = tuningSchemeProperty[tuningSchemeProperties, "t"];
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 3 *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 4c *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  dualMultiplier = getDualMultiplier[tuningSchemeProperties];
  primesErrorMagnitudeNormPower = getDualPower[complexityNormPower];
  
  justSideMappingPart = getPrimesIdentityA[t];
  eitherSideIntervalsPart = Transpose[getPrimesIdentityA[t]];
  powerPart = primesErrorMagnitudeNormPower;
  periodsPerOctavePart = getPeriodsPerOctave[t];
  
  
  If[
    complexitySizeFactor != 0,
    
    AppendTo[generatorsTuningMap, Symbol["gAugmented"]];
    
    m = {Map[Join[#, {0}]&, getA[m]], "co"};
    m = {{getA[m], Join[Table[complexitySizeFactor, Last[Dimensions[getA[m]]] - 1].getLogPrimeCoordinationA[t], {-1}]}, "co"};
    
    AppendTo[primeCentsMap, 0];
    
    justSideMappingPart = basicComplexitySizeFactorAugmentation[justSideMappingPart];
    
    eitherSideIntervalsPart = basicComplexitySizeFactorAugmentation[eitherSideIntervalsPart];
    
    dualMultiplier = basicComplexitySizeFactorAugmentation[dualMultiplier];
  ];
  
  temperedSideGeneratorsPart = generatorsTuningMap;
  temperedSideMappingPart = m;
  justSideGeneratorsPart = primeCentsMap;
  eitherSideMultiplierPart = dualMultiplier;
  
  If[
    logging == True,
    printWrapper["temperedSideGeneratorsPart: ", formatOutput[temperedSideGeneratorsPart]]; (* g *)
    printWrapper["temperedSideMappingPart: ", formatOutput[temperedSideMappingPart]]; (* M *)
    printWrapper["justSideGeneratorsPart: ", formatOutput[justSideGeneratorsPart]]; (* p *)
    printWrapper["justSideMappingPart: ", formatOutput[justSideMappingPart]]; (* I *)
    printWrapper["eitherSideIntervalsPart: ", formatOutput[eitherSideIntervalsPart]]; (* I *)
    printWrapper["eitherSideMultiplierPart: ", formatOutput[eitherSideMultiplierPart]]; (* X⁻¹ *)
    printWrapper["powerPart: ", powerPart];
    printWrapper["periodsPerOctavePart: ", periodsPerOctavePart];
  ];
  
  {
    temperedSideGeneratorsPart, (* g *)
    temperedSideMappingPart, (* M *)
    justSideGeneratorsPart, (* p *)
    justSideMappingPart, (* I *)
    eitherSideIntervalsPart, (* I *)
    eitherSideMultiplierPart, (* X⁻¹ *)
    powerPart,
    periodsPerOctavePart
  }
];

basicComplexitySizeFactorAugmentation[a_] := Module[
  {augmentedA},
  
  augmentedA = Map[Join[#, {0}]&, a];
  AppendTo[augmentedA, Join[Table[0, Last[Dimensions[a]]], {1}]];
  
  augmentedA
];


(* INTERVAL BASIS *)

retrievePrimesIntervalBasisGeneratorsTuningMap[optimumGeneratorsTuningMap_, originalT_, t_] := Module[
  {m, optimumTuningMap, generatorsPreimageTransversal, f},
  
  m = getM[t];
  optimumTuningMap = multiply[{optimumGeneratorsTuningMap, m}, "co"];
  generatorsPreimageTransversal = Transpose[getA[getGeneratorsPreimageTransversalPrivate[originalT]]];
  f = Transpose[getFormalPrimesA[originalT]];
  
  multiply[{optimumTuningMap, f, generatorsPreimageTransversal}, "co"]
];


(* PURE-STRETCHED INTERVAL *)

getPureStretchedIntervalGeneratorsTuningMap[optimumGeneratorsTuningMap_, t_, pureStretchedIntervalV_] := Module[
  {
    generatorsTuningMap,
    m,
    primeCentsMap,
    justIntervalSize,
    temperedIntervalSize
  },
  
  {generatorsTuningMap, m, primeCentsMap} = getTuningSchemeMappings[t];
  
  justIntervalSize = multiply[{primeCentsMap, pureStretchedIntervalV}, "contra"];
  temperedIntervalSize = multiply[{optimumGeneratorsTuningMap, m, pureStretchedIntervalV}, "contra"];
  
  (getA[justIntervalSize] / getA[temperedIntervalSize]) * optimumGeneratorsTuningMap
];


(* TARGETED INTERVAL SETS *)

getOddDiamond[d_] := Module[{oddLimit, oddsWithinLimit, rawDiamond},
  oddLimit = oddLimitFromD[d];
  oddsWithinLimit = Range[1, oddLimit, 2];
  rawDiamond = Map[Function[outer, Map[Function[inner, outer / inner], oddsWithinLimit]], oddsWithinLimit];
  
  (* for when you want the tonality diamond to be in the natural order for a 5-limit diamond, 
  as when developing pedagogical materials and using this library, 
  because it normally doesn't end up getting them in the natural order
  {{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}} *)
  
  padVectorsWithZerosUpToD[Map[quotientToPcv, Map[octaveReduce, Select[DeleteDuplicates[Flatten[rawDiamond]], # != 1&]]], d]
];

octaveReduce[inputI_] := Module[{i},
  i = inputI;
  While[i >= 2, i = i / 2];
  While[i < 1, i = i * 2];
  
  i
];

oddLimitFromD[d_] := Prime[d + 1] - 2;


(* SOLUTIONS: OPTIMIZATION POWER = \[Infinity] (MINIMAX) OR COMPLEXITY NORM POWER = 1 LEADING TO DUAL NORM POWER \[Infinity] ON PRIMES (MAX NORM) *)

(* covers odd-diamond minimax-U "minimax", minimax-S "TOP", pure-stretched-octave minimax-S "POTOP", 
minimax-sopfr-S "BOP", minimax-lil-S "Weil", minimax-lol-S "Kees" *)
(* a semi-analytical solution *)
(* based on https://github.com/keenanpepper/tiptop/blob/main/tiptop.py *)
maxPolytopeSolution[{
  temperedSideGeneratorsPart_,
  temperedSideMappingPart_,
  justSideGeneratorsPart_,
  justSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_,
  powerPart_,
  periodsPerOctavePart_
}, unchangedIntervalsA_] := Module[
  {
    temperedSideButWithoutGeneratorsPart,
    justSide,
    
    generatorCount,
    maxCountOfNestedMinimaxibleDamages,
    minimaxTunings,
    minimaxLockForTemperedSide,
    minimaxLockForJustSide,
    undoMinimaxLocksForTemperedSide,
    undoMinimaxLocksForJustSide,
    uniqueOptimumTuning
  },
  
  (* the mapped and weighted targeted intervals on one side, and the just and weighted targeted intervals on the other;
  note that just side goes all the way down to tuning map level (logs of primes), including the generators
  while the tempered side isn't tuned, but merely mapped. that's so we can solve for the rest of it, 
  i.e. the generators AKA its tunings *)
  temperedSideButWithoutGeneratorsPart = multiply[{temperedSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart}, "co"];
  justSide = getTemperedOrJustSide[justSideGeneratorsPart, justSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart];
  
  (*  Print["temperedSideMappingPart: ", temperedSideMappingPart];
    Print["eitherSideIntervalsPart: ", eitherSideIntervalsPart];
    Print["eitherSideMultiplierPart: ", eitherSideMultiplierPart];
    Print["temperedSideButWithoutGeneratorsPart: ", temperedSideButWithoutGeneratorsPart];
    Print["justSIde: ", justSide];*)
  
  (* our goal is to find the generator tuning map not merely with minimaxed damage, 
  but where the next-highest damage is minimaxed as well, and in fact every next-highest damage is minimaxed, all the way down.
  the tuning which has all damages minimaxed within minimaxed all the way down like this we can call a "nested-minimax".
  it's the only sensible optimum given a desire for minimax damage, so in general we can simply still call it "minimax".
  though people have sometimes distinguished this tuning scheme from the range of minimax tuning schemes with a prefix, 
  such as "TIPTOP tuning" versus "TOP tunings", although there is no value in "TOP tunings" given the existence of "TIPTOP",
  so you may as well just keep calling it "TOP" and refine its definition. anyway...
  
  the `findAllNestedMinimaxTuningsFromPolytopeVertices` function this function calls may come back with more than one result. 
  (sometimes it pulls off some nested-minimaxing on its own, but that's a really subtle point, and we won't worry about it here.)
  the clever way we compute a nested-minimax uses the same polytope vertex searching method used for that first pass, but now with a twist.
  so in the basic case, this method finds the vertices of a max polytope for a temperament.
  so now, instead of running it on the case of the original temperament versus JI, we run it on a distorted version of this case.
  specifically, we run it on a case distorted so that the previous minimaxes are locked down.
  
  we achieve this by picking one of these minimax tunings and offset the just side by it. 
  it doesn't matter which minimax tuning we choose, by the way; they're not sorted, and we simply take the first one.
  the corresponding distortion to the tempered side is trickier, 
  involving the differences between this arbitrarily-chosen minimax tuning and each of the other minimax tunings.
  note that after this distortion, the original rank and dimensionality of the temperament will no longer be recognizable.
  
  we then search for polytope vertices of this minimax-locked distorted situation.
  and we repeatedly do this until we eventually find a unique, nested-minimax optimum. 
  once we've done that, though, our result isn't in the form of a generators tuning map yet. it's still distorted.
  well, with each iteration, we've been keeping track of the distortion applied, so that in the end we could undo them all.
  after undoing those, voilà, we're done! *)
  
  (* the same as rank here, but named this for correlation with elsewhere in this code *)
  (* Print["ehhh, ", temperedSideButWithoutGeneratorsPart];*)
  
  generatorCount = getRPrivate[temperedSideButWithoutGeneratorsPart]; (* TODO: again will need to be switched if un transpose as maybe you should *)
  
  (* this is too complicated to be explained here and will be explained later *)
  maxCountOfNestedMinimaxibleDamages = 0;
  
  (* the candidate generator tuning maps which minimax damage to the targets*)
  (*Print["trying this 1 ", temperedSideButWithoutGeneratorsPart, justSide, maxCountOfNestedMinimaxibleDamages];*)
  minimaxTunings = findAllNestedMinimaxTuningsFromPolytopeVertices[
    temperedSideButWithoutGeneratorsPart,
    justSide,
    maxCountOfNestedMinimaxibleDamages
  ];
  maxCountOfNestedMinimaxibleDamages = generatorCount + 1;
  
  (* no minimax-damage-locking transformations yet, so the transformation trackers are identities 
  per their respective operations of matrix multiplication and addition *)
  undoMinimaxLocksForTemperedSide = {IdentityMatrix[generatorCount], "co"};
  undoMinimaxLocksForJustSide = {Table[{0}, generatorCount], "co"};
  
  While[
    (* a unique optimum has not yet been found *)
    Length[minimaxTunings] > 1,
    
    (* arbitrarily pick one of the minimax damage generator tuning maps; the first one from this unsorted list *)
    minimaxLockForJustSide = First[minimaxTunings];
    (* list of differences between each other minimax generator tuning map and the first one; 
    note how the range starts on index 2 in order to skip the first one *)
    minimaxLockForTemperedSide = {Map[Flatten, Transpose[Map[
      getA[Part[minimaxTunings, #]] - getA[minimaxLockForJustSide]&,
      Range[2, Length[minimaxTunings]]
    ]]], "co"};
    
    (* apply the minimax-damage-locking transformation to the just side, and track it to undo later *)
    (* Print["hows this" , minimaxLockForJustSide,undoMinimaxLocksForTemperedSide,  multiply[{undoMinimaxLocksForTemperedSide, minimaxLockForJustSide}, "co"]];*)
    justSide = {getA[justSide] - getA[multiply[{minimaxLockForJustSide, temperedSideButWithoutGeneratorsPart}, "co"]], "co"};
    undoMinimaxLocksForJustSide = {getA[undoMinimaxLocksForJustSide] + Transpose[getA[multiply[{minimaxLockForJustSide, undoMinimaxLocksForTemperedSide}, "co"]]], "co"};
    
    (* apply the minimax-damage-locking transformation to the tempered side, and track it to undo later *)
    (* this would be a .= if Wolfram supported an analog to += and -= *)
    (* unlike how it is with the justSide, the undo operation is not inverted here; 
    that's because we essentially invert it in the end by left-multiplying rather than right-multiplying *)
    temperedSideButWithoutGeneratorsPart = multiply[{minimaxLockForTemperedSide, temperedSideButWithoutGeneratorsPart }, "co"];
    undoMinimaxLocksForTemperedSide = multiply[{minimaxLockForTemperedSide, undoMinimaxLocksForTemperedSide}, "co"];
    
    (* search again, now in this transformed state *)
    (* Print["trying this 2 ", temperedSideButWithoutGeneratorsPart, justSide, maxCountOfNestedMinimaxibleDamages];*)
    minimaxTunings = findAllNestedMinimaxTuningsFromPolytopeVertices[temperedSideButWithoutGeneratorsPart, justSide, maxCountOfNestedMinimaxibleDamages];
    maxCountOfNestedMinimaxibleDamages += generatorCount + 1;
  ];
  
  uniqueOptimumTuning = First[minimaxTunings];
  
  (*  Print["uniqueOptimumTuning: ", uniqueOptimumTuning];
    Print["undoMinimaxLocksForTemperedSide: ", undoMinimaxLocksForTemperedSide];
    Print["undoMinimaxLocksForJustSide: ", undoMinimaxLocksForJustSide];*)
  
  killian = SetAccuracy[{
    (* here's that left-multiplication mentioned earlier *)
    getA[multiply[{uniqueOptimumTuning, undoMinimaxLocksForTemperedSide}, "co"]] + Transpose[getA[undoMinimaxLocksForJustSide]],
    "co"
  }, 10]; (* TODO: should this 10 be one of our constants *)
  
  (*  Print["killian: ", killian ];*)
  
  killian
];

findAllNestedMinimaxTuningsFromPolytopeVertices[temperedSideButWithoutGeneratorsPart_, justSide_, maxCountOfNestedMinimaxibleDamages_] := Module[
  {
    targetCount,
    generatorCount,
    nthmostMinDamage,
    vertexConstraintAs,
    targetIndices,
    candidateTunings,
    sortedDamagesByCandidateTuning,
    candidateTuning,
    sortedDamagesForThisCandidateTuning,
    newCandidateTunings,
    newSortedDamagesByCandidateTuning
  },
  
  (* in the basic case where no minimax-damage-locking transformations have been applied, 
  these will be the same as the count of original targeted intervals and the rank of the temperament, respectively *)
  targetCount = getD[temperedSideButWithoutGeneratorsPart];
  generatorCount = getR[temperedSideButWithoutGeneratorsPart]; (* TODO: swtiched these but not feeling great about it, and the extra Transpoe[] below *)
  (*  Print["temperedSideButWithoutGeneratorsPart: ", temperedSideButWithoutGeneratorsPart, " and target count: ", targetCount, " and generator count: ", generatorCount];*)
  (*Print["temperedSideButWithoutGeneratorsPart: ", temperedSideButWithoutGeneratorsPart];
   Print["targetCount: ", targetCount];
   Print["generatorCount: ", generatorCount];*)
  
  (* here's the meat of it: solving a linear problem for each vertex of the of tuning polytope;
  more details on this in the constraint matrix gathering function's comments below *)
  candidateTunings = {};
  vertexConstraintAs = getTuningPolytopeVertexConstraintAs[generatorCount, targetCount];
  (* Print["vertexConstraintAs: ", vertexConstraintAs];*)
  Do[
    AppendTo[
      candidateTunings,
      {Quiet[Check[
        (*Print["morpheus: ", multiply[{vertexConstraintA, transpose[temperedSideButWithoutGeneratorsPart]}, "co"]];*)
        (*Print["morpheus: ", multiply[{vertexConstraintA, justSide}, "contra"]];*)
        LinearSolve[
          N[getA[multiply[{vertexConstraintA, transpose[temperedSideButWithoutGeneratorsPart]}, "co"]], linearSolvePrecision],
          N[getA[multiply[{vertexConstraintA, transpose[justSide]}, "co"]], linearSolvePrecision]
        ],
        "err"
      ]], "co"}
    ],
    {vertexConstraintA, vertexConstraintAs}
  ];
  (*  Print["candidateTunings: ", candidateTunings];*)
  (* each damages list is sorted in descending order; 
  the list of lists itself is sorted corresponding to the candidate tunings*)
  sortedDamagesByCandidateTuning = Quiet[Map[
    Function[
      {candidateTuning},
      If[
        ToString[getA[candidateTuning]] == "err",
        "err",
        (*Print["temperedSideButWithoutGeneratorsPart: ", temperedSideButWithoutGeneratorsPart, " and candidateTuning: ", candidateTuning, " and justSide: ", justSide];*)
        (*Print["yoyoyoyoyo: ",multiply[{transpose[candidateTuning], transpose[temperedSideButWithoutGeneratorsPart] }, "co"]];*)
        (*Print["sadgiojdsaogis", getA[multiply[{transpose[candidateTuning], transpose[temperedSideButWithoutGeneratorsPart] }, "co"]] - getA[justSide]];*)
        {Abs[fixUpZeros[getA[multiply[{transpose[candidateTuning], temperedSideButWithoutGeneratorsPart}, "co"]] - getA[justSide]]], "co"}
      ]
    ],
    candidateTunings
  ]];
  
  If[
    debug == True,
    MapThread[
      printWrapper["constraint matrix: ", formatOutput[#1], " tuning: ", formatOutput[#2] , " damages: ", formatOutput[#3]]&,
      {vertexConstraintAs, candidateTunings, sortedDamagesByCandidateTuning}
    ]
  ];
  
  (* ignore the problems that are singular and therefore have no solution *)
  candidateTunings = Select[candidateTunings, !TrueQ[# == {"err", "co"}]&];
  sortedDamagesByCandidateTuning = Select[sortedDamagesByCandidateTuning, !TrueQ[# == "err"]&];
  (* Print["before, ", sortedDamagesByCandidateTuning];*)
  sortedDamagesByCandidateTuning = Map[{{ReverseSort[First[getA[#]]]}, "co"}&, sortedDamagesByCandidateTuning];
  (*Print["after, ", sortedDamagesByCandidateTuning];*)
  
  (*Print["#### LENGHTS MATCH? ", Length[candidateTunings], " VS ", Length[sortedDamagesByCandidateTuning]];*)
  (*     
  here we're iterating by index of the targeted intervals, 
  repeatedly updating the lists candidate tunings and their damages,
  (each pass the list gets shorter, hopefully eventually hitting length 1, at which point a unique tuning has been found,
  but this doesn't necessarily happen, and if it does, it's handled by the function that calls this function)
  until by the final pass they are what we want to return.
  
  there's an inner loop by candidate tuning, and since that list is shrinking each time, the size of the inner loop changes.
  in other words, we're not covering an m \[Times] n rectangular grid's worth of possibilities; more like a jagged triangle.
  
  note that because the damages have all been sorted in descending order,
  these target "indices" do not actually correspond to an individual targeted interval.
  that's okay though because here it's not important which target each of these damages is for.
  all that matters is the size of the damages.
  once we find the tuning we want, we can easily compute its damages list sorted by target when we need it later; that info is not lost.
  
  and note that we don't iterate over *every* target "index".
  we only check as many targets as we could possibly nested-minimax by this point.
  that's why this method doesn't simply always return a unique nested-minimax tuning each time.
  this is also why the damages have been sorted in this way
  so first we compare each tuning's actual minimum damage,
  then we compare each tuning's second-closest-to-minimum damage,
  then compare each third-closest-to-minimum, etc.
  the count of target indices we iterate over is a running total; 
  each time it is increased, it goes up by the present generator count plus 1.
  why it increases by that amount is a bit of a mystery to me, but perhaps someone can figure it out and let me know.
  *)
  targetIndices = Range[Min[maxCountOfNestedMinimaxibleDamages + generatorCount + 1, targetCount]];
  Do[
    (*Print["######## DOING IT FOR ", targetIndex ," OF ", targetIndices];*)
    newCandidateTunings = {};
    newSortedDamagesByCandidateTuning = {};
    
    (* this is the nth-most minimum damage across all candidate tunings,
    where the actual minimum is found in the 1st index, the 2nd-most minimum in the 2nd index,
    and we index it by target index *)
    nthmostMinDamage = Min[Map[Part[First[getA[#]], targetIndex]&, sortedDamagesByCandidateTuning]]; (* TODO: maybe getA[#]? *)
    (*Print["nthmostMinDamage: ", nthmostMinDamage, " which is like ", targetIndex, "  of what???", sortedDamagesByCandidateTuning];*)
    Do[
      (* having found the minimum damage for this target index, we now iterate by candidate tuning index *)
      candidateTuning = Part[candidateTunings, minimaxTuningIndex];
      (*Print["candidateTuning: ", candidateTuning];*)
      sortedDamagesForThisCandidateTuning = Part[sortedDamagesByCandidateTuning, minimaxTuningIndex];
      (* Print["sortedDamagesForThisCandidateTuning: ", sortedDamagesForThisCandidateTuning];*)
      If[
        (* and if this is one of the tunings which is tied for this nth-most minimum damage,
        add it to the list of those that we'll check on the next iteration of the outer loop 
        (and add its damages to the corresponding list) 
        note the tiny tolerance factor added to accommodate computer arithmetic error problems *)
        Part[First[getA[sortedDamagesForThisCandidateTuning]], targetIndex] <= nthmostMinDamage + 0.000000001,
        
        AppendTo[newCandidateTunings, candidateTuning];
        AppendTo[newSortedDamagesByCandidateTuning, sortedDamagesForThisCandidateTuning]
      ],
      
      {minimaxTuningIndex, Range[Length[candidateTunings]]}
    ];
    
    candidateTunings = newCandidateTunings;
    sortedDamagesByCandidateTuning = newSortedDamagesByCandidateTuning,
    
    {targetIndex, targetIndices}
  ];
  
  candidateTunings = Map[{Transpose[getA[#]], "co"}&, candidateTunings];
  
  (* if duplicates are not deleted, then when differences are checked between tunings,
  some will come out to all zeroes, and this causes a crash *)
  shoobee = DeleteDuplicates[
    candidateTunings,
    Function[{tuningA, tuningB}, AllTrue[MapThread[#1 == #2&, {First[getA[tuningA]], First[getA[tuningB]]}], TrueQ]]
  ];
  
  (*Print["shoobee: ", shoobee];*)
  
  shoobee
];
fixUpZeros[l_] := Map[
  Function[
    {nestedList},
    Map[
      If[Quiet[PossibleZeroQ[#]], 0, SetAccuracy[#, linearSolvePrecision]]&,
      nestedList
    ]
  ],
  l
];

getTuningPolytopeVertexConstraintAs[generatorCount_, targetCount_] := Module[ (* TODO: rename its max tuning polytope not just generic *)
  {vertexConstraintA, vertexConstraintAs, targetCombinations, directionPermutations},
  
  vertexConstraintAs = {};
  
  (* here we iterate over every combination of r + 1 (rank = generator count, in the basic case) targets 
  and for each of those combinations, looks at all permutations of their directions. 
  these are the vertices of the maximum damage tuning polytope. each is a generator tuning map. the minimum of these will be the minimax tuning.
  
  e.g. for target intervals 3/2, 5/4, and 5/3, with 1 generator, we'd look at three combinations (3/2, 5/4) (3/2, 5/3) (5/4, 5/3)
  and for the first combination, we'd look at both 3/2 \[Times] 5/4 = 15/8 and 3/2 \[Divide] 5/4 = 6/5.
  
  then what we do with each of those combo perm vertices is build a constraint matrix. 
  we'll apply this constraint matrix to a typical linear equation of the form Ax = b, 
  where A is a matrix, b is a vector, and x is another vector, the one we're solving for.
  in our case our matrix A is M, our mapping, b is our just tuning map j, and x is our generators tuning map g.
  
  e.g. when the targets are just the primes (and thus an identity matrix we can ignore),
  and the temperament we're tuning is 12-ET with M = [12 19 28] and standard interval basis so p = [log₂2 log₂3 log₂5],
  then we have [12 19 28][g₁] = [log₂2 log₂3 log₂5], or a system of three equations:
  
  12g₁ = log₂2
  19g₁ = log₂3
  28g₁ = log₂5
  
  Obviously not all of those can be true, but that's the whole point: we linear solve for the closest possible g₁ that satisfies all well.
  
  Now suppose we get the constraint matrix [1 1 0]. We multiply both sides of the setup by that:
  
  [1 1 0][12 19 28][g₁] = [1 1 0][log₂2 log₂3 log₂5]
  [31][g₁] = [log₂2 + log₂3]
  
  This leaves us with only a single equation:
  
  31g₁ = log₂6
  
  Or in other words, this tuning makes 6/1 pure, and divides it into 31 equal steps.
  If this temperament's mapping says it's 12 steps to 2/1 and 19 steps to 3/1, and it takes 31 steps to a pure 6/1,
  that implies that whatever damage there is on 2/1 is equal to whatever damage there is on 3/1, since they apparently cancel out.
  
  This constraint matrix [1 1 0] means that the target combo was 2/1 and 3/1, 
  because those are the targets corresponding to its nonzero elements.
  And both nonzero elements are +1 meaning that both targets are combined in the same direction.
  If the targeted intervals list had been [3/2, 4/3, 5/4, 8/5, 5/3, 6/5] instead, and the constraint matrix [1 0 0 0 -1 0],
  then that's 3/2 \[Divide] 5/3 = 5/2.
  
  The reason why we only need half of the permutations is because we only need relative direction permutations;
  they're anchored with the first targeted interval always in the super direction.
  *)
  (*Print["generatorCoutn: ", generatorCount];*)
  targetCombinations = DeleteDuplicates[Map[Sort, Select[Tuples[Range[1, targetCount], generatorCount + 1], DuplicateFreeQ[#]&]]];
  
  If[debug == True, printWrapper["targetCombinations: ", formatOutput[targetCombinations]]];
  
  Do[
    (* note that these are only generatorCount, not generatorCount + 1, because whichever is the first one will always be +1 *)
    If[debug == True, printWrapper["  targetCombination: ", formatOutput[targetCombination]]];
    
    directionPermutations = Tuples[{1, -1}, generatorCount];
    If[debug == True, printWrapper["  directionPermutations: ", formatOutput[directionPermutations]]];
    
    Do[
      If[debug == True, printWrapper["    directionPermutation: ", formatOutput[directionPermutation]]];
      
      vertexConstraintA = Table[Table[0, targetCount], generatorCount];
      
      Do[
        vertexConstraintA[[generatorIndex, Part[targetCombination, 1]]] = 1;
        vertexConstraintA[[generatorIndex, Part[targetCombination, generatorIndex + 1]]] = Part[directionPermutation, generatorIndex],
        
        {generatorIndex, Range[generatorCount]}
      ];
      
      If[debug == True, printWrapper["      vertexConstraintA: ", formatOutput[vertexConstraintA]]];
      AppendTo[vertexConstraintAs, vertexConstraintA],
      
      {directionPermutation, directionPermutations}
    ],
    
    {targetCombination, targetCombinations}
  ];
  
  (* if there's only one generator, we also need to consider each tuning where a target is pure 
  (rather than tied for damage with another target) *)
  If[
    generatorCount == 1,
    Do[
      vertexConstraintA = {Table[0, targetCount]};
      vertexConstraintA[[1, targetIndex]] = 1;
      
      AppendTo[vertexConstraintAs, vertexConstraintA],
      
      {targetIndex, Range[targetCount]}
    ]
  ];
  
  (* count should be the product of the indices count and the signs count, plus the r == 1 ones *)
  Map[{#, "co"}&, vertexConstraintAs] (* TODO: rename these to be As inside here, but not outside *)
];


(* SOLUTIONS: OPTIMIZATION POWER = 1 (MINIMSUM) OR COMPLEXITY NORM POWER = \[Infinity] LEADING TO DUAL NORM POWER 1 ON PRIMES (TAXICAB NORM) *)

(* no historically described tuning schemes use this *)
(* an analytical solution *)
(* based on https://en.xen.wiki/w/Target_tunings#Minimax_tuning, 
where odd-diamond minimax-U "minimax" is described;
however, this computation method is in general actually a solution for minisum tuning schemes, not minimax tuning schemes. 
it only lucks out and works for minimax due to the pure-octave-constraint 
and nature of the tonality diamond targeted interval set,
namely that the places where damage to targets are equal is the same where other targets are pure.
*)
sumPolytopeSolution[{
  temperedSideGeneratorsPart_,
  temperedSideMappingPart_,
  justSideGeneratorsPart_,
  justSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_,
  powerPart_,
  periodsPerOctavePart_
}, unchangedIntervalsA_] := Module[
  {
    generatorCount,
    
    unchangedIntervalSetIndices,
    candidateUnchangedIntervalSets,
    normalizedCandidateUnchangedIntervalSets,
    filteredNormalizedCandidateUnchangedIntervalSets,
    candidateOptimumGenerators,
    candidateOptimumGeneratorsTuningMaps,
    candidateOptimumGeneratorTuningMapAbsErrors,
    
    optimumGeneratorsTuningMapIndices,
    optimumGeneratorsTuningMapIndex
  },
  
  generatorCount = First[Dimensions[getA[temperedSideMappingPart]]]; (* First[], not Last[], because it's not transposed here. *) (* TODO: revisit after clean up of max polytope *)
  (* Print["generatorCount: ", generatorCount];*)
  (*Print["eitherSideIntervalsPart: ", eitherSideIntervalsPart];*)
  
  unchangedIntervalSetIndices = Subsets[Range[First[Dimensions[getA[eitherSideIntervalsPart]]]], {generatorCount}];
  candidateUnchangedIntervalSets = Map[{Map[getA[eitherSideIntervalsPart][[#]]&, #], "contra"}&, unchangedIntervalSetIndices];
  normalizedCandidateUnchangedIntervalSets = Map[canonicalFormPrivate, candidateUnchangedIntervalSets];
  (*Print["sdoigajdg", normalizedCandidateUnchangedIntervalSets];
  Print["what it israe", Map[MatrixRank[Transpose[getA[#]]]&,normalizedCandidateUnchangedIntervalSets ]];*)
  filteredNormalizedCandidateUnchangedIntervalSets = DeleteDuplicates[Select[normalizedCandidateUnchangedIntervalSets, MatrixRank[Transpose[getA[#]]] == generatorCount&]];
  candidateOptimumGenerators = Select[Map[
    getGeneratorsAFromUnchangedIntervals[temperedSideMappingPart, #]&,
    filteredNormalizedCandidateUnchangedIntervalSets
  ], Not[# === Null]&];
  candidateOptimumGeneratorsTuningMaps = Map[multiply[{justSideGeneratorsPart, #}, "co"]&, candidateOptimumGenerators];
  candidateOptimumGeneratorTuningMapAbsErrors = Map[
    Total[First[getA[getAbsErrors[{
      #, (* note: this is an override; only reason these approximation parts are unpacked *)
      temperedSideMappingPart,
      justSideGeneratorsPart,
      justSideMappingPart,
      eitherSideIntervalsPart,
      eitherSideMultiplierPart,
      powerPart,
      periodsPerOctavePart
    }]]]]&,
    candidateOptimumGeneratorsTuningMaps
  ];
  
  (* If[
     True == True,*)
  (* printWrapper["candidateUnchangedIntervalSets: ", Map[formatOutput, candidateUnchangedIntervalSets]];
    printWrapper["normalizedCandidateUnchangedIntervalSets: ", Map[formatOutput, normalizedCandidateUnchangedIntervalSets]];
    printWrapper["filteredNormalizedCandidateUnchangedIntervalSets: ", Map[formatOutput,filteredNormalizedCandidateUnchangedIntervalSets]];
    printWrapper["candidateOptimumGenerators: ", Map[formatOutput,candidateOptimumGenerators]];
    printWrapper["candidateOptimumGeneratorsTuningMaps: ", Map[formatOutput,candidateOptimumGeneratorsTuningMaps]];
    printWrapper["candidateOptimumGeneratorTuningMapAbsErrors: ", (*SetPrecision[candidateOptimumGeneratorTuningMapAbsErrors,4],*) SetPrecision[Map[formatOutput,candidateOptimumGeneratorTuningMapAbsErrors],4]]; (*TODO: handle precision in this *)*)
  (*  ];*)
  
  optimumGeneratorsTuningMapIndices = Position[candidateOptimumGeneratorTuningMapAbsErrors, Min[candidateOptimumGeneratorTuningMapAbsErrors]];
  (* Print["dog doin", optimumGeneratorsTuningMapIndices];*)
  If[
    Length[optimumGeneratorsTuningMapIndices] == 1,
    
    (* result is unique; done *)
    optimumGeneratorsTuningMapIndex = First[First[Position[candidateOptimumGeneratorTuningMapAbsErrors, Min[candidateOptimumGeneratorTuningMapAbsErrors]]]];
    candidateOptimumGeneratorsTuningMaps[[optimumGeneratorsTuningMapIndex]],
    
    (* result is non-unique, will need to handle otherwise *)
    Null
  ]
];



getGeneratorsAFromUnchangedIntervals[m_, unchangedIntervalEigenvectors_] := Module[
  {mappedUnchangedIntervalEigenvectors},
  
  (*Print["how we do", m, unchangedIntervalEigenvectors];*)
  
  mappedUnchangedIntervalEigenvectors = multiply[{m, unchangedIntervalEigenvectors}, "contra"];
  
  If[
    Det[getA[mappedUnchangedIntervalEigenvectors]] == 0,
    Null,
    multiply[{unchangedIntervalEigenvectors, inverse[mappedUnchangedIntervalEigenvectors]}, "contra"]
  ]
];


(* SOLUTIONS: OPTIMIZATION POWER = 2 (MINISOS) OR COMPLEXITY NORM POWER = 2 LEADING TO DUAL NORM POWER 2 ON PRIMES (EUCLIDEAN NORM) *)

(* an analytical solution *)
(* covers odd-diamond minisos-U "least squares", minimax-ES "TE", pure-stretched-octave minimax-ES "POTE",
minimax-copfr-ES "Frobenius", minimax-lil-ES "WE", minimax-sopfr-ES "BE" *)
pseudoinverseSolution[{
  temperedSideGeneratorsPart_,
  temperedSideMappingPart_,
  justSideGeneratorsPart_,
  justSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_,
  powerPart_,
  periodsPerOctavePart_
}, unchangedIntervalsA_] := Module[
  {temperedSideButWithoutGeneratorsPart, justSide},
  (*Print["eitherSideIntervalsPart: ", eitherSideIntervalsPart];*)
  temperedSideButWithoutGeneratorsPart = multiply[{temperedSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart}, "co"];
  (*Print["temperedSideButWithoutGeneratorsPart: ", temperedSideButWithoutGeneratorsPart, temperedSideMappingPart,eitherSideIntervalsPart, eitherSideMultiplierPart];*)
  justSide = getTemperedOrJustSide[justSideGeneratorsPart, justSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart];
  
  If[
    debug == True,
    printWrapper["temperedSideButWithoutGeneratorsPart: ", formatOutput[temperedSideButWithoutGeneratorsPart]];
    printWrapper["temperedSideButWithoutGeneratorsPart.Transpose[temperedSideButWithoutGeneratorsPart]: ", formatOutput[multiply[{temperedSideButWithoutGeneratorsPart, temperedSideButWithoutGeneratorsPart}, "co"]]];
    printWrapper["Inverse[temperedSideButWithoutGeneratorsPart.Transpose[temperedSideButWithoutGeneratorsPart]]: ", formatOutput[Inverse[multiply[{temperedSideButWithoutGeneratorsPart, temperedSideButWithoutGeneratorsPart}, "co"]]]];
    printWrapper["Transpose[temperedSideButWithoutGeneratorsPart].Inverse[temperedSideButWithoutGeneratorsPart.Transpose[temperedSideButWithoutGeneratorsPart]]: ", formatOutput[multiply[{temperedSideButWithoutGeneratorsPart, Inverse[multiply[{temperedSideButWithoutGeneratorsPart, temperedSideButWithoutGeneratorsPart}, "co"]]}, "co"]]];
    printWrapper["justSide.Transpose[temperedSideButWithoutGeneratorsPart].Inverse[temperedSideButWithoutGeneratorsPart.Transpose[temperedSideButWithoutGeneratorsPart]]: ", formatOutput[multiply[{justSide, temperedSideButWithoutGeneratorsPart, Inverse[multiply[{temperedSideButWithoutGeneratorsPart, temperedSideButWithoutGeneratorsPart}, "co"]]}, "co"]]];
    printWrapper["justSide: ", formatOutput[justSide]];
  ];
  
  (* Print["oh boy. temperedSideButWithoutGeneratorsPart: ",temperedSideButWithoutGeneratorsPart, " and justSide: ", justSide ];*)
  
  (* Technically the Aᵀ(AAᵀ)⁻¹ type of pseudoinverse is necessary. Wolfram's built-in will sometimes use other techniques, which do not give the correct answer. *)
  jim = multiply[
    {
      justSide,
      multiply[
        {
          transpose[temperedSideButWithoutGeneratorsPart],
          inverse[multiply[
            {
              temperedSideButWithoutGeneratorsPart,
              transpose[temperedSideButWithoutGeneratorsPart]
            },
            "contra"
          ]]
        },
        "co"
      ]
    },
    "co"
  ];
  
  (* Print["dont mess around", jim];*)
  
  jim
];


(* SOLUTIONS: GENERAL OPTIMIZATION POWER (MINISOP) OR GENERAL COMPLEXITY NORM POWER (P-NORM) *)

(* a numerical solution *)
(* covers minimax-lol-ES "KE", unchanged-octave minimax-ES "CTE" *)
powerSumSolution[approximationParts_, unchangedIntervalsA_] := Module[
  {temperedSideGeneratorsPart, solution},
  
  temperedSideGeneratorsPart = approximationParts[approximationParts, "temperedSideGeneratorsPart"];
  
  solution = getPowerSumSolution[approximationParts, unchangedIntervalsA];
  
  (*Print["hello wtf?", solution];*)
  
  {{First[getA[temperedSideGeneratorsPart]] /. Last[solution]}, "co"}
];

(* no historically described tuning schemes use this *)
(* a numerical solution *)
(* this is the fallback for when sumPolytopeSolution fails to find a unique solution *)
powerSumLimitSolution[{
  temperedSideGeneratorsPart_,
  temperedSideMappingPart_,
  justSideGeneratorsPart_,
  justSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_,
  powerPart_,
  periodsPerOctavePart_
}, unchangedIntervalsA_] := Module[
  {
    powerSumPowerLimit,
    powerSumPowerPower,
    powerSumPower,
    previousAbsErrorMagnitude,
    absErrorMagnitude,
    previousSolution,
    solution
  },
  
  powerSumPowerLimit = powerPart;
  powerSumPowerPower = 1;
  powerSumPower = Power[2, 1 / powerSumPowerPower];
  previousAbsErrorMagnitude = 1000001; (* this is just something really big, in order for initial conditions to work *)
  absErrorMagnitude = 1000000; (* this is just something really big, but not quite as big as previous *)
  
  While[
    powerSumPowerPower <= 6 && previousAbsErrorMagnitude - absErrorMagnitude > 0,
    previousAbsErrorMagnitude = absErrorMagnitude;
    previousSolution = solution;
    solution = getPowerSumSolution[{
      temperedSideGeneratorsPart,
      temperedSideMappingPart,
      justSideGeneratorsPart,
      justSideMappingPart,
      eitherSideIntervalsPart,
      eitherSideMultiplierPart,
      powerSumPower, (* note: this is different *)
      periodsPerOctavePart
    }, unchangedIntervalsA];
    absErrorMagnitude = First[solution];
    powerSumPowerPower = powerSumPowerPower += 1;
    powerSumPower = If[powerSumPowerLimit == 1, Power[2, 1 / powerSumPowerPower], Power[2, powerSumPowerPower]];
  ];
  
  myMan = {{First[getA[temperedSideGeneratorsPart]] /. Last[solution]}, "co"};
  
  (* Print["myMan: ", myMan];*)
  
  myMan
];

getPowerSumSolution[approximationParts_, unchangedIntervalsA_] := Module[
  {
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    powerSum,
    minimizedPowerSum
  },
  
  temperedSideGeneratorsPart = approximationPart[approximationParts, "temperedSideGeneratorsPart"];
  temperedSideMappingPart = approximationPart[approximationParts, "temperedSideMappingPart"];
  justSideGeneratorsPart = approximationPart[approximationParts, "justSideGeneratorsPart"];
  justSideMappingPart = approximationPart[approximationParts, "justSideMappingPart"];
  
  powerSum = getPowerSumAbsError[approximationParts];
  (* Print["powerSum", SetPrecision[powerSum,4]];*)
  minimizedPowerSum = SetPrecision[If[
    Length[unchangedIntervalsA] > 0,
    {powerSum, SetPrecision[First[getA[multiply[{temperedSideGeneratorsPart, temperedSideMappingPart, unchangedIntervalsA}, "co"]]] == First[getA[multiply[{justSideGeneratorsPart, justSideMappingPart, unchangedIntervalsA}, "co"]]], nMinimizePrecision]},
    powerSum
  ], nMinimizePrecision];
  
  (* Print["well what am I then ", First[getA[temperedSideGeneratorsPart]]];*)
  myGirl = NMinimize[minimizedPowerSum, First[getA[temperedSideGeneratorsPart]], WorkingPrecision -> nMinimizePrecision];
  
  (* Print["myGirl: ", myGirl];*)
  
  myGirl
];

(* 
where the generators part is ¢1LG (tempered) or ¢1LI (just), the mapping part is M (tempered) or I (just), 
the intervals part is T (finite-target-list) or I (infinite-target-list), and
the multiplier part is W (finite-target-list) or X⁻¹ (infinite-target-list), finds:
tempered finite-target-list:    ¢1LGMTW
tempered infinite-target-list:  ¢1LGMIX⁻¹
just finite-target-list:        ¢1LIITW 
just infinite-target-list:      ¢1LIIIX⁻¹
in the approximation ¢1LGMTW \[TildeTilde] ¢1LIITW or ¢1LGMIX⁻¹ \[TildeTilde] ¢1LIIIX⁻¹
*)
getTemperedOrJustSide[
  temperedOrJustSideGeneratorsPart_,
  temperedOrJustSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_
] := multiply[{temperedOrJustSideGeneratorsPart, temperedOrJustSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart}, "co"];
