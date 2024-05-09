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


generateNotebook = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Skeleton.wlx"}] ];

getNotebook[controls_] := EventFire[controls, "NotebookQ", True] /. {{___, n_Notebook, ___} :> n};

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

buttonTemplate := ImportComponent[FileNameJoin[{rootFolder, "Templates", "Button.wlx"}] ];
AppExtensions`TemplateInjection["AppNotebookTopBar"] = buttonTemplate["HandlerFunction" -> processRequest];

AppExtensions`SidebarIcons = ImportComponent[FileNameJoin[{rootFolder, "Templates", "Icons.wlx"}] ];



(* reader of HTML files *)
{checkEncoding, decode} = ImportComponent[ FileNameJoin[{rootFolder, "Decoder.wl"}] ];

LoaderComponent = ImportComponent[ FileNameJoin[{rootFolder, "Templates", "Loader.wlx"}] ];

Echo["DecoderLoaded!"];
Echo[checkEncoding];

HTMLFileQ[path_] := If[FileExtension[path] === "html", checkEncoding[path], False ];
JerryI`Notebook`Views`Router[any_?HTMLFileQ, appevents_String] := With[{},
    {LoaderComponent[##, "Path"->any, "Decoder"->decode], ""}&
]

End[]
EndPackage[]