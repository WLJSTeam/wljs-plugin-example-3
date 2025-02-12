BeginPackage["CoffeeLiqueur`Extensions`BasicEval`", {
    "CoffeeLiqueur`Notebook`Transactions`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`",
    "CodeParser`"
}]

Needs["CoffeeLiqueur`Notebook`Kernel`" -> "GenericKernel`"];
Needs["CoffeeLiqueur`Notebook`Evaluator`" -> "StandardEvaluator`"];

Begin["`Private`"]

Q[t_Transaction] := StringMatchQ[t["Data"], ".m\n"~~___]



evaluator  = StandardEvaluator`StandardEvaluator["Name" -> "Basic InputForm Evaluator", "Pattern" -> (_?Q), "Priority"->(2)];

rootFolder = $InputFileName // DirectoryName // ParentDirectory;
preload = Import[FileNameJoin[{rootFolder, "src", "Preload.wl"}], "Text"];

StandardEvaluator`ReadyQ[evaluator, k_] := (
    If[! TrueQ[k["ReadyQ"] ] || ! TrueQ[k["ContainerReadyQ"] ],
        EventFire[t, "Error", "Kernel is not ready"];
        Print[evaluator, "Kernel is not ready"];
        False
    ,

        Print[evaluator, "Preload"];

        With[{preload = preload},
            GenericKernel`Init[k, 
                ImportString[preload, "WL"]
            , "Once"->True];
        ];

        True
    ]
);

StandardEvaluator`EvaluateTransaction[evaluator, k_, t_] := Module[{list},
     t["Data"] = StringDrop[t["Data"], 3];

     If[StringLength[StringTrim[t["Data"] ] ] === 0,
        EventFire[t, "Error", "No input"];
        Echo["Syntax Error!"];
        Return[$Failed];
     ];

     With[{check = CheckSyntax[t["Data"] ]},
        If[! TrueQ[check],
            EventFire[t, "Error", check];
            Echo["Syntax Error!"];
            Return[$Failed];
        ];


        list = SplitExpression[t["Data"] ];
        
        MapIndexed[
            With[{message = StringTrim[#1], index = #2[[1]], transaction = Transaction[]},
                If[StringTake[message, -1] === ";", 
                    transaction["Nohup"] = True;
                    transaction["Data"] = StringDrop[message, -1];
                ,
                    transaction["Data"] = message;
                ];
                
                transaction["Evaluator"] = Internal`Kernel`BasicEval;
                
                (* check if it is the last one *)
                If[index === Length[list],
                    EventHandler[transaction, {
                        (* capture successfull event of the last transaction to end the process *)  
                        "Result" -> Function[data, 
                            EventFire[t, "Result", data];
                            EventFire[t, "Finished", True];
                        ],
                        (* fwd the rest *)
                        name_ :> Function[data, EventFire[t, name, data] ]
                    }];          
                ,
                    EventHandler[transaction, {
                        name_ :> Function[data, EventFire[t, name, data] ]
                    }];                
                ];

                Print[evaluator, "Submit transaction!"];
                GenericKernel`SubmitTransaction[k, transaction];
            ]&
        ,  list];
    ];      
];  



SplitExpression[str_] := With[{},
  Select[Select[(StringTake[str, Partition[Join[{1}, #, {StringLength[str]}], 2]] &@
   Flatten[{#1 - 1, #2 + 1} & @@@ 
     Sort@
      Cases[
       CodeParser`CodeConcreteParse[str, 
         CodeParser`SourceConvention -> "SourceCharacterIndex"][[2]], 
       LeafNode[Token`Newline, _, a_] :> Lookup[a, Source, Nothing]]]), StringQ], (StringLength[#]>0) &]
];

CheckSyntax[str_String] := 
    Module[{syntaxErrors = Cases[CodeParser`CodeParse[str],(ErrorNode|AbstractSyntaxErrorNode|UnterminatedGroupNode|UnterminatedCallNode)[___],Infinity]},
        If[Length[syntaxErrors]=!=0 ,
            

            Return[StringRiffle[
                TemplateApply["Syntax error `` at line `` column ``",
                    {ToString[#1],Sequence@@#3[CodeParser`Source][[1]]}
                ]&@@@syntaxErrors

            , "\n"], Module];
        ];
        Return[True, Module];
    ];



End[]
EndPackage[]