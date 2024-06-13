BeginPackage["Notebook`Editor`ExportDecoder`", {
    "JerryI`Notebook`", 
    "JerryI`Misc`Events`",
    "JerryI`WLX`Importer`",
    "JerryI`Misc`Events`Promise`"
}];

Begin["`Internal`"];

{saveNotebook, loadNotebook, renameNotebook, cloneNotebook}         = ImportComponent["Frontend/Loader.wl"];

check[path_String] := Module[{str, result},
    Echo["Notebook`Editor`ExportHTML`Decoder` >> checking encoding"];
    str = OpenRead[path];
    result = Find[str, "<meta serializer=\"hsfn-4\"/>"] =!= EndOfFile;
    Close[str];
    result
]

decodeHTML[path_String] := Module[{str, cells, objects, notebook, store},
With[{
    dir = DirectoryName[path],
    name = FileBaseName[path],
    promise = Promise[],
    start = "<meta serializer=\"hsfn-4\"/><script id=\"json-objects\" type=\"application/json\">{\"storage\":",
    mid1 = "}</script><meta serializer=\"separator\"/><script id=\"cells-data\" type=\"application/json\">{\"storage\":",
    mid2 = "}</script><meta serializer=\"separator\"/><script id=\"json-storage\" type=\"application/json\">{\"storage\":",
    term = "}</script><meta serializer=\"end\"/>"
}, 
    Echo["Notebook`Editor`ExportHTML`Decoder` >> checking encoding"];
    str = OpenRead[path];
    ReadString[str, start];
    ReadList[str, Character, StringLength[start] ];
    objects = ImportString[ReadString[str, mid1], "ExpressionJSON"];
    ReadList[str, Character, StringLength[mid1] ];
    cells = ImportString[ReadString[str, mid2], "ExpressionJSON"];
    ReadList[str, Character, StringLength[mid2] ];
    store = ImportString[ReadString[str, term], "ExpressionJSON"];
    Close[str];


    notebook = <|
        "Notebook" -> <|
            "Objects" -> (<|"Public"->#|>&/@ objects), 
            "Path" -> FileNameJoin[{dir, name<>".wln"}],
            "Storage" -> store
        |>, 
        "Cells" -> (#["Data"] &/@cells["Cells"]),
        "serializer" -> "jsfn4" 
    |>;

    Put[notebook, FileNameJoin[{dir, name<>".wln"}] ];
    EventFire[promise, Resolve, FileNameJoin[{dir, name<>".wln"}] ];
    promise
] ]

lang["mathematica"] := ""
lang["wolfram"] := ""
lang["js"] := ".js\n"
lang["javascript"] := ".js\n"
lang["jsx"] := ".wlx\n"
lang["markdown"] := ".md\n"
lang[any_String] := StringJoin[".", any, "\n"]

decodeMD[path_String] := Module[{str, cells, objects, notebook, store},
With[{
    dir = DirectoryName[path],
    name = FileBaseName[path],
    promise = Promise[]
}, 
   
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
              ]
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
      EventFire[promise, Resolve, FileNameJoin[{dir, name<>".wln"}] ];
    ] ];

   promise 
] ]

End[];    
EndPackage[];

{Notebook`Editor`ExportDecoder`Internal`check, Notebook`Editor`ExportDecoder`Internal`decodeHTML, Notebook`Editor`ExportDecoder`Internal`decodeMD}