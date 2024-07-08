BeginPackage["Notebook`Editor`ExportDecoder`", {
    "JerryI`Notebook`", 
    "JerryI`Misc`Events`",
    "JerryI`WLX`Importer`",
    "JerryI`Misc`Events`Promise`",
    "JerryI`Notebook`Transactions`", 
    "JerryI`Notebook`Kernel`",
    "JerryI`Notebook`LocalKernel`"
}];

Begin["`Internal`"];

System`RowBoxFlatten;

{saveNotebook, loadNotebook, renameNotebook, cloneNotebook}         = ImportComponent["Frontend/Loader.wl"];

check[path_String] := Module[{str, result},
    Echo["Notebook`Editor`ExportHTML`Decoder` >> checking encoding"];
    str = OpenRead[path];
    result = Find[str, "<meta serializer=\"hsfn-4\"/>"] =!= EndOfFile;
    Close[str];
    result
]

decodeHTML[path_String, OptionsPattern[] ] := Module[{str, cells, objects, notebook, store, symbols},
With[{
    dir = DirectoryName[path],
    name = FileBaseName[path],
    promise = Promise[],
    start = "<meta serializer=\"hsfn-4\"/><script id=\"json-objects\" type=\"application/json\">{\"storage\":",
    mid1 = "}</script><meta serializer=\"separator\"/><script id=\"cells-data\" type=\"application/json\">{\"storage\":",
    mid2 = "}</script><meta serializer=\"separator\"/><script id=\"json-storage\" type=\"application/json\">{\"storage\":",
    term = "}</script><meta serializer=\"end\"/>",

    startExtra = "<meta serializer=\"hsfn-extras\"/><script id=\"json-symbols\" type=\"application/json\">{\"storage\":",
    termExtra = "}</script><meta serializer=\"end-extras\"/>",

    spinner = Notifications`Spinner["Topic"->"Converting to notebook", "Body"->"Please, wait"](*`*),
    msg = OptionValue["Messager"]
}, 
    EventFire[msg, spinner, True];

    Echo["Notebook`Editor`ExportHTML`Decoder` >> checking encoding"];
    str = OpenRead[path];
    ReadString[str, start];
    ReadList[str, Character, StringLength[start] ];
    objects = ImportString[ReadString[str, mid1], "ExpressionJSON"];
    ReadList[str, Character, StringLength[mid1] ];
    cells = ImportString[ReadString[str, mid2], "ExpressionJSON"];
    ReadList[str, Character, StringLength[mid2] ];
    store = ImportString[ReadString[str, term], "ExpressionJSON"];

    ReadString[str, startExtra];
    ReadList[str, Character, StringLength[startExtra] ];
    symbols = ReadString[str, termExtra];
    If[symbols === EndOfFile, symbols = <||>,
      symbols = ImportString[symbols, "ExpressionJSON"];
    ];
    
    Close[str];


    notebook = <|
        "Notebook" -> <|
            "Objects" -> (<|"Public"->#|>&/@ objects), 
            "Path" -> FileNameJoin[{dir, name<>".wln"}],
            "Storage" -> store,
            "Symbols" -> symbols
        |>, 
        "Cells" -> (#["Data"] &/@cells["Cells"]),
        "serializer" -> "jsfn4" 
    |>;

    Put[notebook, FileNameJoin[{dir, name<>".wln"}] ];
    EventFire[spinner["Promise"], Resolve, True];
    EventFire[promise, Resolve, FileNameJoin[{dir, name<>".wln"}] ];
    promise
] ]

Options[decodeHTML] = {"Messager"->"", "Client"->Null}

lang["mathematica"] := ""
lang["wolfram"] := ""
lang["js"] := ".js\n"
lang["javascript"] := ".js\n"
lang["jsx"] := ".wlx\n"
lang["markdown"] := ".md\n"
lang[any_String] := StringJoin[".", any, "\n"]

decodeMD[path_String, OptionsPattern[] ] := Module[{str, cells, objects, notebook, store},
With[{
    dir = DirectoryName[path],
    name = FileBaseName[path],
    promise = Promise[],
    msg = OptionValue["Messager"],
    client = OptionValue["Client"],
    spinner = Notifications`Spinner["Topic"->"Converting to notebook", "Body"->"Please, wait"](*`*)
}, 
    EventFire[msg, spinner, True];

    str = Import[path, "Text"];


    notebook = Notebook[];
    With[{n = notebook},
        n["Path"] = FileNameJoin[{dir, name<>".wln"}];
    ];


    With[{list = With[{s = StringSplit[
          str, 
          p : (("```"~~WordCharacter..~~"\n") | ("```")) -> p
        ]},
          SequenceReplace[Map[Function[ss,
            With[{t = StringTrim[ss]},
              With[{tag = StringTake[t, Min[3, StringLength[t]]]},
                {StringTrim[tag] === "```", t}
              ]
            ]
          ], s] // Flatten, {True, c_, False, b_, True, d_} :> codeBlock[StringDrop[c, 3], b] ]
      ]},
      Map[
        Function[
          item, 
          Switch[
            Head[item],
            codeBlock,

              CellObj[
                "Data" -> StringJoin[lang[item[[1]] // StringTrim], item[[2]]], 
                "Type" -> "Input", 
                "Notebook" -> notebook
              ],

            String,
              CellObj[
                "Data" -> StringJoin[".md\n", item], 
                "Type" -> "Input", 
                "Notebook" -> notebook, 
                "Props" -> <|"Hidden" -> True|>
              ];
              CellObj[
                "Data" -> item, 
                "Type" -> "Output", 
                "Display" -> "markdown", 
                "Notebook" -> notebook
              ];
            ,
            _,
              Echo["skip"];
          ]
        ], 
        list
      ]
    ];    

    Echo["SAVING////////"];
    Then[saveNotebook[notebook], Function[Null,
      EventFire[spinner["Promise"], Resolve, True];
      EventFire[promise, Resolve, FileNameJoin[{dir, name<>".wln"}] ];
    ] ];

   promise 
] ]

Options[decodeMD] = {"Messager"->"", "Client"->Null}

processString[str_String] := StringReplace[ExportString[str, "String"], "\\[NoBreak]"->""]

convert[Cell[BoxData[boxes_List], "Input", ___], notebook_, kernel_] := With[{p = Promise[]},
  Then[evaluateInPlace[StringRiffle[ToString[# /. {RowBox->RowBoxFlatten}] &/@ boxes, ""], kernel], Function[reply,
    CellObj["Data"->processString[reply["Data"] ], "Type"->"Input", "Notebook"->notebook ];
    EventFire[p, Resolve, True];
  ] ];
  p
]

convert[Cell[BoxData[boxes_], "Input", ___], notebook_, kernel_] := With[{p = Promise[]},
  Then[evaluateInPlace[StringRiffle[ToString[# /. {RowBox->RowBoxFlatten}] &/@ {boxes}, ""], kernel], Function[reply,
    CellObj["Data"->processString[reply["Data"] ], "Type"->"Input", "Notebook"->notebook ];
    EventFire[p, Resolve, True];
  ] ];
  p
]


takeFirst[expr_, ___] := expr

toStringFormExperimental[boxes_] := (
  Echo[ToString[boxes, InputForm] ];
  boxes /. {StyleBox -> takeFirst}
)

convert[Cell[data_, "Text", ___], notebook_, kernel_] := (
  CellObj["Data"->StringJoin[".md\n", ToString[data /. {StyleBox[dta_, ___] :> dta}] ], "Type"->"Input", "Notebook"->notebook , "Props"-><|"Hidden"->True|>];
  CellObj["Data"->ToString[data /. {StyleBox[dta_, ___] :> dta}], "Display"->"markdown", "Type"->"Output", "Notebook"->notebook ];
  
)

convert[Cell[t: TextData[data_], "Text", ___], notebook_, kernel_] := (
  CellObj["Data"->StringJoin[".md\n", toStringFormExperimental[data] ], "Type"->"Input", "Notebook"->notebook , "Props"-><|"Hidden"->True|>];
  CellObj["Data"->toStringFormExperimental[data], "Display"->"markdown", "Type"->"Output", "Notebook"->notebook ];

)

convert[Cell[TextData[data_RowBox], "Text", ___], notebook_, kernel_] := With[{},
  CellObj["Data"->StringJoin[".md\n", ToString[data /. {RowBox -> StringJoin} /. {StyleBox[dta_, ___] :> dta}] ], "Type"->"Input", "Notebook"->notebook , "Props"-><|"Hidden"->True|>];
  CellObj["Data"->ToString[data /. {RowBox -> StringJoin} /. {StyleBox[dta_, ___] :> dta}], "Display"->"markdown", "Type"->"Output", "Notebook"->notebook ];

]

convert[Cell[BoxData[boxes_List], "Output", ___], notebook_, kernel_] := With[{p = Promise[]},

  Then[evaluateInPlace[StringRiffle[ToString[ToExpression[#, StandardForm], StandardForm]& /@ boxes, ""] , kernel], Function[reply,
    CellObj["Data"->processString[reply["Data"] ], "Type"->"Input", "Notebook"->notebook ];
    EventFire[p, Resolve, True];
  ] ];
  p
]



convert[Cell[BoxData[boxes_], "Output", ___], notebook_, kernel_] := With[{p = Promise[]},

  Then[evaluateInPlace[StringRiffle[ToString[ToExpression[#, StandardForm], StandardForm]& /@ {boxes}, ""] , kernel], Function[reply,
    CellObj["Data"-> processString[reply["Data"] ], "Type"->"Input", "Notebook"->notebook ];
    EventFire[p, Resolve, True];
  ] ];
  p
]

ApplySync[f_, w_, {first_, rest___}, final_] := f[w@@first, Function[Null, Echo["Async >> Next"]; ApplySync[f,w, {rest}, final]]]
ApplySync[f_, w_, {}, final_] := final[];

convert[Cell[CellGroupData[list_List, ___], ___], notebook_, kernel_] := With[{p = Promise[]},
  ApplySync[Then, convert, {
          #, notebook, kernel
      } &/@ list, Function[Null,
      
      EventFire[p, Resolve, True];
  ] ];  
  p
]

evaluateInPlace[expr_, k_] := With[{t = Transaction[], p = Promise[]},
    t["Evaluator"] = Internal`Kernel`Evaluator`Held ;
    t["Data"] = Hold[expr];

    EventHandler[t, {
        (* capture successfull event of the last transaction to end the process *)  
        "Result" -> Function[data, 
            EventFire[p, Resolve, data];
        ]
    }];      

    Kernel`Submit[k, t];
    p
]

SetAttributes[evaluateInPlace, HoldFirst]


decodeMathematica[opts__][path_String, secondaryOpts___] := Module[{
  str, cells, objects, notebook, nb, store, options
},
With[{
    dir = DirectoryName[path],
    name = FileBaseName[path],
    promise = Promise[],
    spinner = Notifications`Spinner["Topic"->"Converting to notebook", "Body"->"Please, wait"](*`*)
}, 
    Echo["Convering Mathematica Notebook..."];
    nb = Import[path];


    notebook = Notebook[];
    With[{n = notebook},
        n["Path"] = FileNameJoin[{dir, name<>".wln"}];
    ];

    
    options = Join[Association[List[opts] ], Association[ List[secondaryOpts] ] ]; 

    If[Length[options["Kernels"] //ReleaseHold ] === 0,
      EventFire[options["Messager"], "Error", "The converting process is not possible without working Kernels"];
      Return[promise];
    ];

    Print["requesting modal...."];
      With[{request = CreateUUID[]},
        EventHandler[request, {
            "Success" -> Function[data,
                
                If[TrueQ[data["ContainerReadyQ"] ],
                    Echo[data];

                    notebook["Evaluator"] = data["Container"];
                    EventFire[notebook, "AquairedKernel", True];

                    EventFire[options["Messager"], spinner, True];

                    ApplySync[Then, convert, {
                      #, notebook, data
                    } &/@ nb[[1]], Function[Null,
      
                      Echo["SAVING////////"];
                      Then[saveNotebook[notebook], Function[Null,
                        EventFire[promise, Resolve, FileNameJoin[{dir, name<>".wln"}] ];
                      ] ];
                    ] ]; 

                ,
                    EventFire[options["Messager"], "Error", "Container is not ready! Try again later"];
                ];
                
                EventRemove[request];
            ],
            
            "Error" -> Function[error,
                        EventFire[options["Messager"], "Error", "Error while selecting"];
                        EventRemove[request];
            ],
            
            _ -> Function[Null,
                        EventFire[options["Messager"], "Error", "The converting is not possible without a working Kernel"];
                        EventRemove[request];
            ]
        }];
        
        Print["fire!"];
        EventFire[options["Modals"], "SuggestKernel", <|"Client"->options["Client"], "Callback"->request, "Kernels"->(options["Kernels"] //ReleaseHold)|>];    
    ];

    (**)

   promise 
] ]

End[];    
EndPackage[];

{Notebook`Editor`ExportDecoder`Internal`check, Notebook`Editor`ExportDecoder`Internal`decodeHTML, Notebook`Editor`ExportDecoder`Internal`decodeMD, Notebook`Editor`ExportDecoder`Internal`decodeMathematica}