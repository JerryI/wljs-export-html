BeginPackage["Notebook`Editor`MathematicaEncoder`", {
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

(* better use this https://community.wolfram.com/groups/-/m/t/2142852 *)

transformHeadings[str_] := Which[
    StringMatchQ[str, "# "~~__],
    TextCell[StringDrop[str, 2], "Title"],

    StringMatchQ[str, "## "~~__],
    TextCell[StringDrop[str, 3], "Section"],

    StringMatchQ[str, "#### "~~__],
    TextCell[StringDrop[str, 4], "Subsection"],

    StringMatchQ[str, "##### "~~__],
    TextCell[StringDrop[str, 2], "Subsubsection"],

    True,
    Print[str];
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

splitMarkdown[cell_] := mergeMarkdown[transformHeadings /@ StringSplit[cell["Data"], "\n"] ]

convertCell[cell_] := splitMarkdown[cell] /; ((cell["Display"] === "markdown") && OutputCellQ[cell])
convertCell[cell_] := Nothing /; ((cell["Display"] =!= "markdown") && OutputCellQ[cell])

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

convertCell[cell_] := ToExpression[cell["Data"], InputForm, convertToBoxes]  /; (language[cell["Data"] ] === "wolfram" && InputCellQ[cell])
convertCell[cell_] := Cell[ dropFirstLine[ cell["Data"] ], "Code" ] /; (language[cell["Data"] ] =!= "wolfram" && language[cell["Data"] ] =!= "markdown" && InputCellQ[cell])

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

Notebook`Editor`MathematicaEncoder`Internal`encode