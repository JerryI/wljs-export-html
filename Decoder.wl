BeginPackage["CoffeeLiqueur`Extensions`ExportImport`Decoder`", {
    "JerryI`Misc`Events`",
    "JerryI`WLX`Importer`",
    "JerryI`Misc`Events`Promise`",
    "CoffeeLiqueur`Notebook`Transactions`"
}];


Begin["`Internal`"];

Needs["CoffeeLiqueur`Notebook`Kernel`" -> "GenericKernel`"];
Needs["CoffeeLiqueur`Notebook`LocalKernel`" -> "LocalKernel`"]

Needs["CoffeeLiqueur`Notebook`Cells`" -> "cell`"];
Needs["CoffeeLiqueur`Notebook`" -> "nb`"];

System`RowBoxFlatten; (* needed to fix Kernel and Master definitions *)

{saveNotebook, loadNotebook, renameNotebook, cloneNotebook}         = ImportComponent["Frontend/Loader.wl"];

(*                                             ***                                                 *)
(*                                         HTML Converter                                          *)
(*                                             ***                                                 *)

check[path_String] := Module[{str, result},
    Echo["CoffeeLiqueur`Extensions`Editor`ExportHTML`Decoder` >> checking encoding"];
    str = OpenRead[path];
    result = Find[str, "<meta serializer=\"hsfn-4\"/>"] =!= EndOfFile;
    Close[str];
    result
]

decodeHTML[path_String | path_File, OptionsPattern[] ] := Module[{str, cells, objects, notebook, store, symbols, place},
With[{
    dir = AppExtensions`QuickNotesDir,
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

    Echo["CoffeeLiqueur`Extensions`Editor`ExportHTML`Decoder` >> checking encoding"];
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
            "Storage" -> store,
            "Symbols" -> symbols,
            "HaveToSaveAs" -> True,
            "Quick" -> True
        |>, 
        "Cells" -> (#["Data"] &/@cells["Cells"]),
        "serializer" -> "jsfn4" 
    |>;

    place = FileNameJoin[{dir, name<>StringTake[CreateUUID[], 3]<>".wln"}];
    Put[notebook,  place];
    
    EventFire[spinner["Promise"], Resolve, True];
    EventFire[promise, Resolve,  place];
    promise
] ]

Options[decodeHTML] = {"Messager"->"", "Client"->Null}

(*                                             ***                                                 *)
(*                                       Markdown Converter                                        *)
(*                                             ***                                                 *)

lang["mathematica"] := ""
lang["wolfram"] := ""
lang["wl"] := ""
lang["js"] := ".js\n"
lang["javascript"] := ".js\n"
lang["jsx"] := ".wlx\n"
lang["html"] := ".html\n"
lang["reveal"] := ".slide\n"
lang["bash"] := ".sh\n"
lang["shell"] := ".sh\n"
lang["mermaid"] := ".mermaid\n"
lang["sh"] := ".sh\n"
lang["revealjs"] := ".slide\n"
lang["markdown"] := ".md\n"
lang[any_String] := StringJoin[".", any, "\n"]

codeBlock;
codeBlock["mermaid", rest_] := codeBlockInPlace["mermaid", rest] 

fixImages[s_String] := With[{},
  StringReplace[s, {
    RegularExpression["!(\\[[\\w|\\d|-| |_]*\\])\\(([^\\(
)\\[\\]]*)\\)"] :> With[{
    label = "$1",
    url = "$2"
},
      If[StringTake[url, 1] == "/",
        StringTemplate["![``](``)"][label, url]
      ,
        If[StringTake[url, Min[4, StringLength[url] ] ] == "http",
          StringTemplate["![``](``)"][label, url]
        ,
          StringTemplate["![``](/``)"][label, url ]
        ]
        
      ]
    ]
  
  }]
]

decodeMD[path_String, OptionsPattern[] ] := Module[{str, cells, objects, notebook, store},
With[{
    dir = AppExtensions`QuickNotesDir,
    name = FileBaseName[path],
    promise = Promise[],
    msg = OptionValue["Messager"],
    client = OptionValue["Client"],
    spinner = Notifications`Spinner["Topic"->"Converting to notebook", "Body"->"Please, wait"](*`*)
}, 
    EventFire[msg, spinner, True];

    str = Import[path, "Text"];


    notebook = nb`NotebookObj[];
    With[{n = notebook},
        n["Quick"] = True;
        n["HaveToSaveAs"] = True;
        n["WorkingDirectory"] = DirectoryName[path];
        n["Path"] = FileNameJoin[{dir, name<>StringTake[CreateUUID[], 3]<>".wln"}];
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

              cell`CellObj[
                "Data" -> StringJoin[lang[item[[1]] // StringTrim], item[[2]]], 
                "Type" -> "Input", 
                "Notebook" -> notebook
              ],

            codeBlockInPlace,
              cell`CellObj[
                "Data" -> StringJoin[lang[item[[1]] // StringTrim], item[[2]]], 
                "Type" -> "Input", 
                "Notebook" -> notebook, 
                "Props" -> <|"Hidden" -> True|>
              ];
              cell`CellObj[
                "Data" -> item[[2]], 
                "Type" -> "Output", 
                "Display" -> "mermaid", 
                "Notebook" -> notebook
              ];
            ,


            String,
              cell`CellObj[
                "Data" -> StringJoin[".md\n", fixImages @ item], 
                "Type" -> "Input", 
                "Notebook" -> notebook, 
                "Props" -> <|"Hidden" -> True|>
              ];
              cell`CellObj[
                "Data" -> fixImages[item], 
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
    Then[saveNotebook[notebook["Path"], notebook, "NoCache"->True], Function[Null,

      EventFire[spinner["Promise"], Resolve, True];
      EventFire[promise, Resolve, notebook["Path"] ];
      Delete /@ notebook["Cells"];
      Delete[notebook];
    ] ];

   promise 
] ]

Options[decodeMD] = {"Messager"->"", "Client"->Null}

(*                                             ***                                                 *)
(*                                   Mathematica NB Converter                                      *)
(*                                             ***                                                 *)

processString[str_String] := StringReplace[ExportString[str, "String"], "\\[NoBreak]"->""]

convert[Cell[BoxData[boxes_List], "Input", ___], notebook_, kernel_] := With[{p = Promise[]},
  Then[evaluateInPlace[StringRiffle[ToString[# /. {RowBox->RowBoxFlatten}] &/@ boxes, ""], kernel], Function[reply,
    cell`CellObj["Data"->processString[reply["Data"] ], "Type"->"Input", "Notebook"->notebook ];
    EventFire[p, Resolve, True];
  ] ];
  p
]

convert[Cell[input_String, "Input", ___], notebook_, kernel_] := With[{},
  cell`CellObj["Data"->processString[input ], "Type"->"Input", "Notebook"->notebook ];
]

convert[Cell[BoxData[boxes_], "Input", ___], notebook_, kernel_] := With[{p = Promise[]},
  Then[evaluateInPlace[StringRiffle[ToString[# /. {RowBox->RowBoxFlatten}] &/@ {boxes}, ""], kernel], Function[reply,
    cell`CellObj["Data"->processString[reply["Data"] ], "Type"->"Input", "Notebook"->notebook ];
    EventFire[p, Resolve, True];
  ] ];
  p
]


takeFirst[expr_, ___] := expr

toStringFormExperimental[boxes_] := Module[{},
  Echo[ToString[boxes, InputForm] ];
  With[{r = ToExpression[boxes, StandardForm, HoldForm]},
    If[r == $Failed,
      Echo[">> Decoder >> Convertion Failed at: "];
      Echo[ToString[boxes, InputForm] ];
      Return[""];
    ];

    

    StringJoin[" ", ToString[r], " "]
  ]


]

toStringFormExperimental[s_String] := s

toStringFormExperimental[boxes_BoxData] := Module[{r},
  r = ToExpression[boxes[[1]], StandardForm, HoldForm];
  If[r == $Failed,
    Echo[">> Decoder >> Convertion Failed at: "];
    Echo[ToString[boxes, InputForm] ];
    Return[""];
  ];
  StringJoin[" $", ToString[r // TeXForm, InputForm], "$ "]
]

toStringFormExperimental[boxes_Cell] := toStringFormExperimental[boxes[[1]]]

stringTest[s_String] := s
stringTest[_] := ""

convert[Cell[data_, "Text", ___], notebook_, kernel_] := (
  cell`CellObj["Data"->stringTest[StringJoin[".md\n", ToString[data /. {StyleBox[dta_, ___] :> dta}] ] ], "Type"->"Input", "Notebook"->notebook , "Props"-><|"Hidden"->True|>];
  cell`CellObj["Data"->stringTest[ToString[data /. {StyleBox[dta_, ___] :> dta}] ], "Display"->"markdown", "Type"->"Output", "Notebook"->notebook ];
  
)

convert[Cell[t: TextData[data_], "Text", ___], notebook_, kernel_] := (
  cell`CellObj["Data"->stringTest[StringJoin[".md\n", toStringFormExperimental[data] ] ], "Type"->"Input", "Notebook"->notebook , "Props"-><|"Hidden"->True|>];
  cell`CellObj["Data"->stringTest[toStringFormExperimental[data] ], "Display"->"markdown", "Type"->"Output", "Notebook"->notebook ];
)

convert[Cell[t: TextData[data_List], "Text", ___], notebook_, kernel_] := (
  cell`CellObj["Data"->stringTest[StringJoin[".md\n", StringJoin@@(ToString /@ toStringFormExperimental /@ data) ] ], "Type"->"Input", "Notebook"->notebook , "Props"-><|"Hidden"->True|>];
  cell`CellObj["Data"->stringTest[StringJoin@@(ToString /@ toStringFormExperimental /@ data)], "Display"->"markdown", "Type"->"Output", "Notebook"->notebook ];
)

convert[Cell[text_String, "Subsubsection", ___], notebook_, kernel_] := (
  cell`CellObj["Data"->StringJoin[".md\n#### ", text], "Type"->"Input", "Notebook"->notebook , "Props"-><|"Hidden"->True|>];
  cell`CellObj["Data"->StringJoin["#### ", text], "Display"->"markdown", "Type"->"Output", "Notebook"->notebook ];
)

convert[Cell[text_String, "Subsection", ___], notebook_, kernel_] := (
  cell`CellObj["Data"->StringJoin[".md\n### ", text], "Type"->"Input", "Notebook"->notebook , "Props"-><|"Hidden"->True|>];
  cell`CellObj["Data"->StringJoin["### ", text], "Display"->"markdown", "Type"->"Output", "Notebook"->notebook ];
)

convert[Cell[text_String, "Section", ___], notebook_, kernel_] := (
  cell`CellObj["Data"->StringJoin[".md\n## ", text], "Type"->"Input", "Notebook"->notebook , "Props"-><|"Hidden"->True|>];
  cell`CellObj["Data"->StringJoin["## ", text], "Display"->"markdown", "Type"->"Output", "Notebook"->notebook ];
)

convert[Cell[text_String, "Title", ___], notebook_, kernel_] := (
  cell`CellObj["Data"->StringJoin[".md\n# ", text], "Type"->"Input", "Notebook"->notebook , "Props"-><|"Hidden"->True|>];
  cell`CellObj["Data"->StringJoin["# ", text], "Display"->"markdown", "Type"->"Output", "Notebook"->notebook ];
)


convert[Cell[TextData[data_RowBox], "Text", ___], notebook_, kernel_] := With[{},
  cell`CellObj["Data"->stringTest[StringJoin[".md\n", ToString[data /. {RowBox -> StringJoin} /. {StyleBox[dta_, ___] :> dta}] ] ], "Type"->"Input", "Notebook"->notebook , "Props"-><|"Hidden"->True|>];
  cell`CellObj["Data"->stringTest[ToString[data /. {RowBox -> StringJoin} /. {StyleBox[dta_, ___] :> dta}] ], "Display"->"markdown", "Type"->"Output", "Notebook"->notebook ];

]

convert[Cell[BoxData[boxes_List], "Output", ___], notebook_, kernel_] := With[{p = Promise[]},

  Then[evaluateInPlace[StringRiffle[ToString[ToExpression[#, StandardForm], StandardForm]& /@ boxes, ""] , kernel], Function[reply,
    cell`CellObj["Data"->processString[reply["Data"] ], "Type"->"Output", "Notebook"->notebook ];
    EventFire[p, Resolve, True];
  ] ];
  p
]



convert[Cell[BoxData[boxes_], "Output", ___], notebook_, kernel_] := With[{p = Promise[]},

  Then[evaluateInPlace[StringRiffle[ToString[ToExpression[#, StandardForm], StandardForm]& /@ {boxes}, ""] , kernel], Function[reply,
    cell`CellObj["Data"-> processString[reply["Data"] ], "Type"->"Output", "Notebook"->notebook ];
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

    GenericKernel`SubmitTransaction[k, t];
    p
]

SetAttributes[evaluateInPlace, HoldFirst]


decodeMathematica[opts__][path_String, secondaryOpts___] := Module[{
  str, cells, objects, notebook, nb, store, options
},
With[{
    dir = AppExtensions`QuickNotesDir,
    name = FileBaseName[path],
    promise = Promise[],
    spinner = Notifications`Spinner["Topic"->"Converting to notebook", "Body"->"Please, wait"](*`*)
}, 
    Echo["Convering Mathematica Notebook..."];
    nb = Import[path];


    notebook = nb`NotebookObj[];
    With[{n = notebook},
        n["Quick"] = True;
        n["HaveToSaveAs"] = True;    
        n["WorkingDirectory"] = DirectoryName[path];
        n["Path"] = FileNameJoin[{dir, name<>StringTake[CreateUUID[], 3]<>".wln"}];
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
                      Then[saveNotebook[notebook["Path"], notebook, "NoCache"->True], Function[Null,
                        EventFire[promise, Resolve, notebook["Path"] ];
                        Delete /@ notebook["Cells"];
                        Delete[notebook];
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

{CoffeeLiqueur`Extensions`ExportImport`Decoder`Internal`check, CoffeeLiqueur`Extensions`ExportImport`Decoder`Internal`decodeHTML, CoffeeLiqueur`Extensions`ExportImport`Decoder`Internal`decodeMD, CoffeeLiqueur`Extensions`ExportImport`Decoder`Internal`decodeMathematica}