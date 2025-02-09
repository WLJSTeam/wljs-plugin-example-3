BeginPackage["CoffeeLiqueur`Extensions`BasicEval`", {
    "JerryI`Misc`Events`"
}]

Begin["`Private`"]

Internal`Kernel`BasicEval = Function[t, 
    With[{hash = CreateUUID[]},
        With[{result = (ToExpression[ t["Data"], InputForm, Hold]) // ReleaseHold },
            If[KeyExistsQ[t, "Nohup"],
                EventFire[Internal`Kernel`Stdout[ t["Hash"] ], "Result", <|"Data" -> Null |> ];
            ,   
                With[{string = ToString[result, InputForm]},
                    EventFire[Internal`Kernel`Stdout[ t["Hash"] ], "Result", <|"Data" -> string, "Display"->"codemirror", "Meta"->Sequence["Hash"->hash] |> ];
                ]
            ];
        ];
    ] 
];

End[]
EndPackage[]