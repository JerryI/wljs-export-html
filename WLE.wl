BeginPackage["Notebook`Editor`WLEDecoder`", {
    "JerryI`Notebook`", 
    "JerryI`Notebook`Windows`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`Async`",
    "JerryI`WLX`Importer`",
    "JerryI`Misc`Events`Promise`",
    "JerryI`Notebook`Transactions`", 
    "JerryI`Notebook`Kernel`",
    "JerryI`Notebook`LocalKernel`"
}];

Begin["`Internal`"];


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
    
    notebook = Notebook`Deserialize[ Import[path, "WL"] ],

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
                initCells = Select[Select[notebook["Cells"], InputCellQ], (#["Props"]["InitGroup"] === True) &],
                last = FirstCase[notebook["Cells"] // Reverse, _?InputCellQ]
            },
                EventFire[spinner["Promise"], Resolve, True];

                Kernel`Init[kernel,
                    Notebook`CellOperations`Private`spinners[generated] = Notebook`Utils`Notifications`Notify["Evaluating cells in the generated context", "Topic"->"Notebook", "Type"->"Spinner"];
                    $ContextPath = $ContextPath /. "Global`" -> Nothing;
                    $Context = generated;
                    $ContextPath = Append[$ContextPath, generated];
                ];

                CellObj`Evaluate[#] &/@ initCells;


                With[{hash = kernel["Hash"], s = Promise[] // First},
                    Then[Promise[s], Function[Null,
                        With[{win = WindowObj["Notebook" -> notebook, "Data" -> last["Data"], "Ref" -> last["Hash"] ]},
                            Echo["project >> sending global event"];
                            EventFire[notebook, "OnWindowCreate", <|"Window"->win, "Client"->options["Client"]|>];
                            EventHandler[win["Hash"] // EventClone, {"Ready" -> Function[Null,
                                Kernel`Init[kernel,

                                    $ContextPath = Append[$ContextPath /. generated -> Nothing, "Global`"];
                                    $Context = "Global`";
                                ];                            
                            ]}];

                            EventFire[promise, Resolve, {StringJoin["/window?id=", win["Hash"] ], ""} ];
                        
                        ] ;                    
                    ] ];

                    Kernel`Init[kernel,
                        Delete[Notebook`CellOperations`Private`spinners[generated] ];
                        Unset[Notebook`CellOperations`Private`spinners[generated] ];
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

{Notebook`Editor`WLEDecoder`Internal`execute}