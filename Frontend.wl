BeginPackage["Notebook`Editor`ExportHTML`", {
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

rootFolder = $InputFileName // DirectoryName;
AppExtensions`TemplateInjection["SettingsFooter"] = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Settings.wlx"}] ];


generateNotebook = ImportComponent[FileNameJoin[{rootFolder, "Templates", "HTML.wlx"}] ];
generateMarkdown = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Markdown.wlx"}] ];

getNotebook[controls_] := EventFire[controls, "NotebookQ", True] /. {{___, n_Notebook, ___} :> n};

exportMarkdown[controls_, modals_, messager_, client_, notebookOnLine_Notebook, path_, name_] := With[{

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

exportHTML[controls_, modals_, messager_, client_, notebookOnLine_Notebook, path_, name_] := With[{

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
                    Export[filename, generateNotebook["Root"->rootFolder, "Notebook" -> notebookOnLine, "Title"->name] // ToStringRiffle, "Text"];
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
        name = FileBaseName[ notebookOnLine["Path"] ]
    },
        With[{
            p = Promise[]
        }, 
            EventFire[modals, "Select", <|"Client"->client, "Promise"->p, "Title"->"Which format", "Options"->{"HTML", "Markdown"}|>];
            Then[p, Function[choise,
                {exportHTML, exportMarkdown}[[choise["Result"] ]][controls, modals, messager, client, notebookOnLine, path, name];
            ] ];
        ]
    ]
]

buttonTemplate := ImportComponent[FileNameJoin[{rootFolder, "Templates", "Button.wlx"}] ];
AppExtensions`TemplateInjection["AppNotebookTopBar"] = buttonTemplate["HandlerFunction" -> processRequest];

AppExtensions`SidebarIcons = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Icons.wlx"}] ];



(* reader of HTML and MD files *)
{checkEncoding, decodeHTML, decodeMD, decodeMathematica} = ImportComponent[ FileNameJoin[{rootFolder, "Decoder.wl"}] ];

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