BeginPackage["CoffeeLiqueur`Extensions`ExportImport`Widgets`", {
    "JerryI`Misc`Events`",
    "JerryI`Misc`Async`",
    "JerryI`WLX`Importer`",
    "JerryI`Misc`Events`Promise`",
    "CoffeeLiqueur`Notebook`Transactions`"
}];


Begin["`Internal`"];

Needs["CoffeeLiqueur`Notebook`Kernel`" -> "GenericKernel`"];
Needs["CoffeeLiqueur`Notebook`LocalKernel`" -> "LocalKernel`"]

Needs["CoffeeLiqueur`Notebook`Windows`" -> "win`"];

Needs["CoffeeLiqueur`Notebook`Cells`" -> "cell`"];
Needs["CoffeeLiqueur`Notebook`" -> "nb`"];



(*                                             ***                                                 *)
(*                                         WLE Decoder                                          *)
(*                                             ***                                                 *)

checkKernel[kernel_, cbk_] := (Echo["Checking kernel..."]; If[TrueQ[kernel["ContainerReadyQ"] ] && TrueQ[kernel["ReadyQ"] ],
    Echo["Kernel is ready!"];
    cbk[kernel];
,
    Echo["Not yet..."];
    SetTimeout[checkKernel[kernel, cbk], 1000];
])

execute[opts__][path_String, secondaryOpts___] := Module[{str, cells, objects, notebook, store, symbols, place},
With[{
    dir = AppExtensions`QuickNotesDir,
    name = FileBaseName[path],
    promise = Promise[],
    
    notebook = nb`Deserialize[ Import[path, "WL"] ],

    spinner = Notifications`Spinner["Topic"->"Initializing an App", "Body"->"Please, wait"](*`*),
    msg = OptionValue["Messager"],
    generated = RandomWord[]<>"`"
}, 

    options = Join[Association[List[opts] ], Association[ List[secondaryOpts] ] ]; 

    EventFire[msg, spinner, True];

    If[Length[options["Kernels"] //ReleaseHold ] === 0,
      EventFire[spinner["Promise"], Resolve, True];
      EventFire[options["Messager"], "Error", "The process is not possible to start without working Kernels"];
      Pause[2];

      Return[promise];
    ];

    

    With[{kernel = options["Kernels"] //ReleaseHold //First},
        checkKernel[kernel, Function[data,

            notebook["Evaluator"] = data["Container"];
            EventFire[notebook, "AquairedKernel", True];

            Echo["Starting evaluation", "WLE Decoder"];
            With[{
                initCells = Select[Select[notebook["Cells"], cell`InputCellQ], (#["Props"]["InitGroup"] === True) &],
                last = FirstCase[notebook["Cells"] // Reverse, _?cell`InputCellQ]
            },
                EventFire[spinner["Promise"], Resolve, True];

                GenericKernel`Init[kernel,
                    CoffeeLiqueur`Extensions`RemoteCells`Private`spinners[generated] = CoffeeLiqueur`Extensions`Notifications`Notify["Evaluating cells in the generated context", "Topic"->"Notebook", "Type"->"Spinner"];
                    $ContextPath = $ContextPath /. "Global`" -> Nothing;
                    $Context = generated;
                    $ContextPath = Append[$ContextPath, generated];
                ];

                cell`EvaluateCellObj[#] &/@ initCells;


                With[{hash = kernel["Hash"], s = Promise[] // First},
                    Then[Promise[s], Function[Null,
                        With[{win = win`WindowObj["Notebook" -> notebook, "Data" -> last["Data"], "Ref" -> last["Hash"] ]},
                            Echo["project >> sending global event"];
                            EventFire[notebook, "OnWindowCreate", <|"Window"->win, "Client"->options["Client"]|>];
                            EventHandler[win["Hash"] // EventClone, {"Ready" -> Function[Null,
                                GenericKernel`Init[kernel,

                                    $ContextPath = Append[$ContextPath /. generated -> Nothing, "Global`"];
                                    $Context = "Global`";
                                ];                            
                            ]
                            }];

                            EventFire[promise, Resolve, {StringJoin["/window?id=", win["Hash"] ], ""} ];
                        
                        ] ;                    
                    ] ];

                    GenericKernel`Init[kernel,
                        Delete[CoffeeLiqueur`Extensions`RemoteCells`Private`spinners[generated] ];
                        Unset[CoffeeLiqueur`Extensions`RemoteCells`Private`spinners[generated] ];
                        EventFire[Internal`Kernel`Stdout[ s ], Resolve, True ]; 
                    ];


                ];
            ];

            
        ] ];
    ];


    
    

    promise
] ]


End[];    
EndPackage[];

{CoffeeLiqueur`Extensions`ExportImport`Widgets`Internal`execute}