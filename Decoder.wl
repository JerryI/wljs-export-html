BeginPackage["Notebook`Editor`ExportHTMLDecoder`", {
    "JerryI`Notebook`", 
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`"
}];

Begin["`Internal`"];

check[path_String] := Module[{str, result},
    Echo["Notebook`Editor`ExportHTML`Decoder` >> checking encoding"];
    str = OpenRead[path];
    result = Find[str, "<meta serializer=\"hsfn-4\"/>"] =!= EndOfFile;
    Close[str];
    result
]

decode[path_String] := Module[{str, cells, objects, notebook, store},
With[{
    dir = DirectoryName[path],
    name = FileBaseName[path],
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
    FileNameJoin[{dir, name<>".wln"}]
] ]

End[];    
EndPackage[];

{Notebook`Editor`ExportHTMLDecoder`Internal`check, Notebook`Editor`ExportHTMLDecoder`Internal`decode}