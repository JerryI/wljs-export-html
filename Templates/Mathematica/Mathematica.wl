BeginPackage["CoffeeLiqueur`Extensions`ExportImport`Mathematica`", {

    "JerryI`Misc`Events`",
    "JerryI`Misc`Async`",
    "JerryI`WLX`Importer`",
    "JerryI`Misc`Events`Promise`",
    "CoffeeLiqueur`Notebook`Transactions`"
}];


Begin["`Internal`"];

Needs["CoffeeLiqueur`Notebook`Kernel`" -> "GenericKernel`"];
Needs["CoffeeLiqueur`Notebook`LocalKernel`" -> "LocalKernel`"]

Needs["CoffeeLiqueur`Notebook`Cells`" -> "cell`"];
Needs["CoffeeLiqueur`Notebook`" -> "nb`"];

(* better use this https://community.wolfram.com/groups/-/m/t/2142852 *)

transformHeadings[str_] := Which[
    StringMatchQ[str, "# "~~__],
    Cell[StringDrop[str, 2], "Title"],

    StringMatchQ[str, "## "~~__],
    Cell[StringDrop[str, 3], "Section"],

    StringMatchQ[str, "#### "~~__],
    Cell[StringDrop[str, 4], "Subsection"],

    StringMatchQ[str, "##### "~~__],
    Cell[StringDrop[str, 5], "Subsubsection"],

    True,
    str                
]

dropFirstLine[s_String] := With[{l = StringSplit[s, "\n"]},
    If[Length[l] > 1,
         StringRiffle[Drop[l, 1], "\n"]
    ,
        s
    ]
]

mergeMarkdown[list_List] := Replace[SequenceReplace[list, {s1_String, s2__String} :> StringRiffle[{s1,s2}, "\n"] ], s_String :> Cell[s, "Text"], 1];

splitMarkdown[cell_cell`CellObj] := mergeMarkdown[transformHeadings /@ StringSplit[cell["Data"], "\n"] ]

OutputMarkdownQ[cell_cell`CellObj] := (cell["Display"] === "markdown") && cell`OutputCellQ[cell];
InputWolframQ[cell_cell`CellObj] := cell`InputCellQ[cell] && (language[cell["Data"] ] === "wolfram");
InputCodeCellQ[cell_cell`CellObj] := cell`InputCellQ[cell] && (language[cell["Data"] ] =!= "markdown" && language[cell["Data"] ] =!= "wolfram");


convertCell[cell_cell`CellObj?OutputMarkdownQ] := splitMarkdown[cell];
convertCell[cell_cell`CellObj] := Nothing;

language[s_String] := Which[
    StringMatchQ[s, ".md\n"~~__],
    "markdown",

    StringMatchQ[s, ".wlx\n"~~__],
    "wlx",

    StringMatchQ[s, ".html\n"~~__],
    "html",        

    StringMatchQ[s, ".js\n"~~__],
    "js",  

    StringMatchQ[s, ".mermaid\n"~~__],
    "mermaid",     

    StringMatchQ[s, ".slide\n"~~__],
    "slides",      

    StringMatchQ[s, "."~~(WordCharacter..)~~"\n"~~___],
    "unknown",         

    StringMatchQ[s, (WordCharacter..)~~"."~~(WordCharacter..)~~"\n"~~___],
    "unknown", 

    True,
    "wolfram"
]

convertToBoxes[s_] := With[{m = ToString[Unevaluated[s], InputForm]}, Cell[m, "Input" ] ]
convertToBoxes[s__] := convertToBoxes /@ Unevaluated[{s}]
SetAttributes[convertToBoxes, HoldAllComplete];
SetAttributes[convertToBoxes, Listable];

convertCell[cell_cell`CellObj?InputWolframQ] := ToExpression[cell["Data"], InputForm, convertToBoxes]  
convertCell[cell_cell`CellObj?InputCodeCellQ] := Cell[ dropFirstLine[ cell["Data"] ], "CodeText" ] 

convertCell[_] := Nothing

encode[path_, OptionsPattern[] ] := With[{
    notebook = OptionValue["Notebook"]
},
    With[{n = {Map[convertCell, notebook["Cells"] ] }// Flatten},
        Put[Notebook[n], path]
    ]
]

Options[encode] = {"Root"->"", "Notebook" -> "", "Title"->""}

End[];
EndPackage[];

CoffeeLiqueur`Extensions`ExportImport`Mathematica`Internal`encode