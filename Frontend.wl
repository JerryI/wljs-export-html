BeginPackage["Notebook`Editor`ExportNotebook`", {
    "JerryI`Notebook`", 
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`",
    "JerryI`WLX`",
    "JerryI`WLX`Importer`",
    "JerryI`WLX`WebUI`", 
    "JerryI`Notebook`AppExtensions`",
    "JerryI`Misc`WLJS`Transport`",
    "JerryI`WLJSPM`"
}]

Begin["`Internal`"]

compress[str_String] := With[{arr = Normal[ExportByteArray[str, "String"] ]},
 BaseEncode[ByteArray[Developer`RawCompress[arr](*`*) ] ]
];

rootFolder = $InputFileName // DirectoryName;
AppExtensions`TemplateInjection["SettingsFooter"] = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Settings.wlx"}] ];

{loadSettings, storeSettings}        = ImportComponent["Frontend/Settings.wl"];


settings = <||>;

generateNotebook = ImportComponent[FileNameJoin[{rootFolder, "Templates", "HTML", "Static.wlx"}] ];
generateDynamicNotebook= ImportComponent[FileNameJoin[{rootFolder, "Templates", "HTML", "Dynamic.wlx"}] ];
generateSlides   = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Slides", "Static.wlx"}] ];
generateMarkdown = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Markdown", "Markdown.wlx"}] ];

getNotebook[controls_] := EventFire[controls, "NotebookQ", True] /. {{___, n_Notebook, ___} :> n};

exportSlides[controls_, modals_, messager_, client_, notebookOnLine_Notebook, path_, name_, ext_] := With[{

},
    With[{

    },
        
        With[{
            p = Promise[]
        },
            EventFire[modals, "RequestPathToSave", <|
                "Promise"->p,
                "Title"->"Full page slides",
                "Ext"->"html",
                "Client"->client
            |>];

            Then[p, Function[result, 
                Module[{filename = result<>".html"},
                    If[filename === ".html", filename = name<>filename];
                    If[DirectoryName[filename] === "", filename = FileNameJoin[{path, filename}] ];

    
                    With[{slidesQ = Map[Function[cell, cell["Display"] == "slide" ], notebookOnLine["Cells"] ]},
                        If[!(Or @@ slidesQ),
                            EventFire[messager, "Warning", "Notebook does not contain any output slide cells"];
                            Return[];                        
                        ]
                    ];

                    Export[filename, generateSlides["Root"->rootFolder, "Notebook" -> notebookOnLine, "Title"->name] // ToStringRiffle, "Text"];
                    EventFire[messager, "Saved", "Exported to "<>filename];
                ];
            ], Function[result, Echo["!!!R!!"]; Echo[result] ] ];
            
        ]
    ]
]

exportMarkdown[controls_, modals_, messager_, client_, notebookOnLine_Notebook, path_, name_, ext_] := With[{

},
    With[{

    },
        
        With[{
            p = Promise[]
        },
            EventFire[modals, "RequestPathToSave", <|
                "Promise"->p,
                "Title"->"Markdown",
                "Ext"->"md",
                "Client"->client
            |>];

            Then[p, Function[result, 
                Module[{filename = result<>".md"},
                    If[filename === ".md", filename = name<>filename];
                    If[DirectoryName[filename] === "", filename = FileNameJoin[{path, filename}] ];
                    Export[filename, generateMarkdown["Root"->rootFolder, "Notebook" -> notebookOnLine, "Title"->name] // ToStringRiffle, "Text"];
                    EventFire[messager, "Saved", "Exported to "<>filename];
                ];
            ], Function[result, Echo["!!!R!!"]; Echo[result] ] ];
            
        ]
    ]
]

figuresModal = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Modal", "Figures.wlx"}] ];
figuresCodeModal = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Modal", "FiguresCode.wlx"}] ];
{figuresTemplate, figuresHead} = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Figures", "Template.wlx"}] ];


exportFigures[controls_, modals_, messager_, client_, notebookOnLine_Notebook, path_, name_, ext_] := With[{
    objects = notebookOnLine["Objects"],
    sym     = notebookOnLine["Symbols"]
},
    With[{
        p = Promise[]
    },
        EventFire[modals, "CustomModal", <|
            "Promise"->p,
            "Data" -> Keys[objects],
            "Content" -> figuresModal,
            "Client"->client
        |>];

        Then[p, Function[result,
            With[{object = Values[objects][[result, "Public"]]},
                loadSettings[settings];

                If[AssociationQ[sym] && TrueQ[Length[Keys[sym] ] > 0],

                    With[{request = CreateUUID[]},
                        EventHandler[request, {
                            "Success" -> Function[Null, 
                                EventRemove[request];

                                With[{
                                    code = figuresTemplate["Expression" -> object, "Symbols" -> sym],
                                    head = figuresHead["Settings" -> settings]
                                },
                                    EventFire[modals, "CustomModal", <|
                                        "Promise"->Null,
                                        "Data" -> <|"Code"->code, "Head"->head|>,
                                        "Content" -> figuresCodeModal,
                                        "Client"->client
                                    |>];
                                ]                        
                            ],

                            _ -> Function[Null,
                                EventRemove[request];
                                With[{
                                    code = figuresTemplate["Expression" -> object],
                                    head = figuresHead["Settings" -> settings]
                                },
                                    EventFire[modals, "CustomModal", <|
                                        "Promise"->Null,
                                        "Data" -> <|"Code"->code, "Head"->head|>,
                                        "Content" -> figuresCodeModal,
                                        "Client"->client
                                    |>];
                                ]                                 
                            ]
                        }];

                        EventFire[modals, "GenericAskTemplate", <|
                            "Callback" -> request, 
                            "Client" -> client,
                            "Title" -> "Should we include dynamic symbols?", 
                            "Content" -> StringJoin["We found ", ToString[Length[Keys[sym] ] ], " symbols stored in the notebook."],
                            "SVGIcon" -> With[{},
                                ""
                            ]
                        |>];

                ];
                ,
                    With[{
                        code = figuresTemplate["Expression" -> object],
                        head = figuresHead["Settings" -> settings]
                    },
                        EventFire[modals, "CustomModal", <|
                            "Promise"->Null,
                            "Data" -> <|"Code"->code, "Head"->head|>,
                            "Content" -> figuresCodeModal,
                            "Client"->client
                        |>];
                    ]                
                ];
            ]
        ] ];
        
    ]
]


Notebook`Editor`ExportNotebook`Internal`Sniffer;
Notebook`Editor`ExportNotebook`Internal`Sampler;

analyser = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Analyser", "Analyser.wlx"}] ];
sampler = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Analyser", "Sampler.wlx"}] ];

sampling[controls_, modals_, messager_, client_, notebookOnLine_Notebook, path_, name_, ext_] := Module[{
    notification
}, With[{
    channel = CreateUUID[]
},

    

    EventHandler[channel, {
        "Abort" -> Function[Null,
            WebUISubmit[Sampler["Stop",  channel], client];
            WebUISubmit[Sampler["Dispose",  channel], client];
            WebUISubmit[Sniffer["Purge", channel], client];
            Delete[notification];
        ],

        "Continue" -> Function[Null,
            Then[WebUIFetch[Sampler["Get",  channel], client, "Format"->"RawJSON"], Function[payload,
                
                
                With[{compressed = compress /@ payload},
                
                    WebUISubmit[Sampler["Stop",  channel], client];
                    WebUISubmit[Sampler["Dispose",  channel], client];
                    WebUISubmit[Sniffer["Purge", channel], client];
                    
                    Delete[notification];

                    With[{ratio = Round[ByteCount[payload] / ByteCount[compressed], 0.1] },
                        EventFire[messager, Notifications`NotificationMessage["Info"], "Compressed by <span class=\"font-semibold\">"<>ToString[ratio]<>"</span>"]
                        Echo["Compressed by the factor of "<>ToString[ratio] ]; 
                        

                        With[{
                            p = Promise[]
                        },
                            EventFire[modals, "RequestPathToSave", <|
                                "Promise"->p,
                                "Title"->"Portable notebook",
                                "Ext"->"html",
                                "Client"->client
                            |>];

                            Then[p, Function[result, 
                                Module[{filename = result<>".html"},
                                    If[filename === ".html", filename = name<>filename];
                                    If[DirectoryName[filename] === "", filename = FileNameJoin[{path, filename}] ];
                                    Export[filename, generateDynamicNotebook["Root"->rootFolder, "ExtensionTemplates" -> ext, "Compressed"->compressed, "Notebook" -> notebookOnLine, "Title"->name] // ToStringRiffle, "Text"];
                                    EventFire[messager, "Saved", "Exported to "<>filename];
                                ];
                            ], Function[result, Echo["!!!R!!"]; Echo[result] ] ];

                        ]                       
                    ];



                    
                ]

            ] ];
        ]
    }];

    WebUISubmit[Sampler["Init",  channel], client];

    notification = Notifications`Custom["Topic"->"Sampling the data", "Body"->sampler["Channel"->channel, "Client"->client, "Log"->messager, "Notebook"->notebookOnLine], "Controls"->False];
    EventFire[messager, notification, True];
] ]

exportDynamicHTML[controls_, modals_, messager_, client_, notebookOnLine_Notebook, path_, name_, ext_] := Module[{notification, dump, raw, groups}, With[{
    sniffer = CreateUUID[]
},
    If[notebookOnLine["Evaluator"]["Kernel"]["State"] =!= "Initialized", 
        EventFire[messager, "Error", "Exporting dynamic HTML document requires connected Kernel and initialized notebook session."];
        Return[];
    ];


    EventHandler[sniffer, {
        "Continue" -> Function[Null,
            WebUISubmit[Sniffer["Eject", sniffer], client];
            sampling[controls, modals, messager, client, notebookOnLine, path, name, ext];

            Delete[notification];
        ],

        "Abort" -> Function[Null,
            WebUISubmit[Sniffer["Eject", sniffer], client];
            WebUISubmit[Sniffer["Purge", sniffer], client];
            Delete[notification];
        ]
    }];

    EventFire[messager, Notifications`Beeper[], True];
    
    WebUISubmit[Sniffer["Inject", sniffer], client];
    notification = Notifications`Custom["Topic"->"Analysing dynamic bindings", "Body"->analyser["Sniffer"->sniffer, "Client"->client, "Log"->messager, "Notebook"->notebookOnLine], "Controls"->False];
    EventFire[messager, notification, True];

] ]


exportHTML[controls_, modals_, messager_, client_, notebookOnLine_Notebook, path_, name_, ext_] := With[{

},
    With[{

    },
        
        With[{
            p = Promise[]
        },
            EventFire[modals, "RequestPathToSave", <|
                "Promise"->p,
                "Title"->"Portable notebook",
                "Ext"->"html",
                "Client"->client
            |>];

            Then[p, Function[result, 
                Module[{filename = result<>".html"},
                    If[filename === ".html", filename = name<>filename];
                    If[DirectoryName[filename] === "", filename = FileNameJoin[{path, filename}] ];
                    Export[filename, generateNotebook["Root"->rootFolder, "ExtensionTemplates" -> ext, "Notebook" -> notebookOnLine, "Title"->name] // ToStringRiffle, "Text"];
                    EventFire[messager, "Saved", "Exported to "<>filename];
                ];
            ], Function[result, Echo["!!!R!!"]; Echo[result] ] ];
            
        ]
    ]
]

processRequest[controls_, modals_, messager_, client_] := With[{
    notebookOnLine = getNotebook[controls]
},
    With[{
        path = DirectoryName[ notebookOnLine["Path"] ],
        name = FileBaseName[ notebookOnLine["Path"] ],
        ext  = AppExtensions`Templates
    },
        With[{
            p = Promise[]
        }, 
            EventFire[modals, "Select", <|"Client"->client, "Promise"->p, "Title"->"Which format", "Options"->{"Static HTML","Dynamic HTML (experimental)", "Markdown", "Only slides", "Only figures"}|>];
            Then[p, Function[choise,
                EventFire[messager, Notifications`NotificationMessage["Info"], "Collecting static data"];
                With[{tt = EventFire[notebookOnLine, "OnBeforeSave", <|"Client" -> client|>]},
                    Then[tt, Function[Null,
                        {exportHTML, exportDynamicHTML, exportMarkdown, exportSlides, exportFigures}[[choise["Result"] ]][controls, modals, messager, client, notebookOnLine, path, name, ext];
                    ] ] 
                ];
            ] ];
        ]
    ]
]

buttonTemplate := ImportComponent[FileNameJoin[{rootFolder, "Templates", "Button.wlx"}] ];
AppExtensions`TemplateInjection["AppNotebookTopBar"] = buttonTemplate["HandlerFunction" -> processRequest];

AppExtensions`SidebarIcons = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Icons.wlx"}] ];



(* reader of HTML and MD files *)
{checkEncoding, decodeHTML, decodeMD, decodeMathematica} = ImportComponent[ FileNameJoin[{rootFolder, "Decoder.wl"}] ];

EventHandler[AppExtensions`AppProtocol, {
    "open_html" -> Function[assoc,
        Echo[">> Handling URL protocol ! >>"];
        Echo[assoc["url"] ];
        Module[{path = assoc["url"]},
            If[StringTake[path, 5] === "file:",
                Echo["Local file!"];
                path = If[$OperatingSystem === "Windows",
                    FileNameJoin[StringSplit[StringDrop[path, 6], "/"] ],
                    "/"<>FileNameJoin[StringSplit[StringDrop[path, 5], "/"] ]
                ];
                Echo["Local path:"];
                Echo[path];
            ,
                Echo["Web resource"];
                Echo["Downloading..."];
                path = URLDownload[path];
            ];

            With[{p = decodeHTML[path, "Messager"->assoc["Messanger"], "Client"->assoc["Client"] ]},
                Echo[p];
                Then[p, Function[result,
                  Pause[1];
                  Echo[result];
                  (*/*EventFire[spinner["Promise"], Resolve, True];*/*)
                  WebUILocation[ StringJoin["/", URLEncode[ result ] ], assoc["Client"] ] // Echo;

                ] ];
            ];
        ];
    ]
}]

LoaderComponent = ImportComponent[ FileNameJoin[{rootFolder, "Templates", "Loader.wlx"}] ];

Echo["DecoderLoaded!"];
Echo[checkEncoding];

HTMLFileQ[path_] := If[FileExtension[path] === "html", checkEncoding[path], False ];
JerryI`Notebook`Views`Router[any_?HTMLFileQ, appevents_String] := With[{},
    {LoaderComponent[##, "Path"->any, "Decoder"->decodeHTML], ""}&
]

MDFileQ[path_] := FileExtension[path] === "md"

JerryI`Notebook`Views`Router[any_?MDFileQ, appevents_String] := With[{},
    {LoaderComponent[##, "Path"->any, "Decoder"->decodeMD], ""}&
]

NBFileQ[path_] := FileExtension[path] === "nb"

JerryI`Notebook`Views`Router[any_?NBFileQ, appevents_String] := With[{},
    Echo["Mathematica Notebook!"];
    Echo[decodeMathematica[##] ];

    {LoaderComponent[##, "Path"->any, "Decoder"->decodeMathematica[##] ], ""}
]&



End[]
EndPackage[]