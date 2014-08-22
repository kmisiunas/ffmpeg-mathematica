(* ::Package:: *)

(* Authors:
   Karolis Misiunas (km558@cam.ac.uk) 

 *)


(* ::Text:: *)
(**)


(*The package provides methods for importing/exporting video using ffmpeg library.
  Problem with Mathematica's Import function is artefacts it produced using QuickTime.
  The aim of the library is to be as compatible as possible with original Mathematica's
  functions with prefix of FF-...

  Designed for B&W videos.
  *)
(*Version 1 (2014-05-07) - initial release. *)


(* ::Section:: *)
(* Package Declarations*)


BeginPackage["FFmpeg`"];


FFImport::usage = 
	"FFImport[\"file\", elements] loads the parameters necessary for the Import.
  |  If supplied with {\"Frames\", 1} or {\"Frames\", Range[]} it will use ffmpeg
  |  to fetch frames. Optimised for loading consecutive frames."

FFExport::usage = 
  "FFExport[\"file.ext\", expr] will export list of frames as a video file (todo)."

FFmpeg::usage = 
  "FFmpeg[] returns status of the plug-in. 
  |If text argument is supplied it is assumed to be path to ffmpeg."

FFInputStreamAt::usage = 
  ""

FFGetNextFrame::usage = 
  ""

FFSkipFrame::usage = 
  ""



(* options associated*)
Options[FFmpeg] = { 
  "Colors" -> 3 (*number of color channels*),
  "ColorCommand" -> "rgb24" (*indicator for "-pix_fmt" parameter: gray/rgb24*)
}


(* ::Section:: *)
(*Package Implementations*)


Begin["`Private`"]


(* ::Subsection::Closed:: *)
(*FFmpeg Function*)


(*set the path to ffmpeg*)
FFmpeg[path_String] := (ffmpeg = path;)

(*returns status of ffmpeg.*)
(*todo: on windows it responds, but does not checkout.*)
FFmpeg[] := 
  If[ !StringQ@ffmpeg,
    Print @ "The path to ffmpeg is unknown. Use FFmpeg[\"path\"] to set it.",
    (*second option - test if working*)
    If[ StringMatchQ[ 
        ToString @ ReadLine @ OpenRead["!" ~~ ffmpeg ~~ " -version", BinaryFormat -> True],
        "ffmpeg version*"],
      Print @ "ffmpeg was found and is functional",
      Print @ ("ffmpeg does not respond correctly. Please check the path: " <> ToString@ffmpeg) 
    ]
  ]

(*run on loading - default path*)
Switch[ $OperatingSystem, 
  "MacOSX",  FFmpeg @ "/usr/local/bin/ffmpeg",
  "Windows", FFmpeg @ "ffmpeg.exe",
  "Linux",   FFmpeg @ "ffmpeg"];


(* ::Subsection:: *)
(*FFmpeg Implementation*)


 (*reads stream for next frame*)
FFGetNextFrame[stream_, dim_] := 
  Image[
    Partition[ 
      Partition[ 
        BinaryReadList[stream, "Byte", OptionValue[FFmpeg, "Colors"]*dim[[1]]*dim[[2]] ]
        , OptionValue[FFmpeg, "Colors"] ]
      , dim[[1]] ]
  , "Byte"]

(*skip frame*)
FFSkipFrame[stream_, dim_, n_Integer:1] := Skip[ stream, Byte, OptionValue[FFmpeg, "Colors"]*n*dim[[1]]*dim[[2]] ]


(*makes a stream*)
FFInputStreamAt[file_String, at_Integer, noOfFrames_Integer] := 
  Module[{fps, startAtSec, st, dim},
  fps = Import[file, "FrameRate"];
  dim = Import[file, "ImageSize"];
  startAtSec = (at-1) / fps;
  st = OpenRead["!" ~~ ffmpeg ~~ " -i " ~~ file ~~ 
    " -ss " ~~ ToString@startAtSec ~~ (* method too slow!*)
    " -frames:v " ~~ ToString@noOfFrames ~~ 
    " -loglevel quiet" ~~ 
    " -f image2pipe " ~~ 
    " -pix_fmt " ~~ OptionValue[FFmpeg, "ColorCommand"] ~~ 
    " -vcodec rawvideo -", 
    BinaryFormat -> True];
    (* FFSkipFrame[st, dim, at-1]; *)
  {st, dim}
];

(*makes a stream*)
FFInputStreamAt[file_String, at_Integer, All] := 
  Module[{fps, startAtSec, st, dim},
  fps = Import[file, "FrameRate"];
  dim = Import[file, "ImageSize"];
  startAtSec = (at-1) / fps;
  st = OpenRead["!" ~~ ffmpeg ~~ " -i " ~~ file ~~ 
    " -ss " ~~ ToString@startAtSec ~~ (* method too slow!*)
    (*" -frames:v " ~~ ToString@noOfFrames ~~ *)
    " -loglevel quiet" ~~ 
    " -f image2pipe " ~~ 
    " -pix_fmt " ~~ OptionValue[FFmpeg, "ColorCommand"] ~~ 
    " -vcodec rawvideo -", 
    BinaryFormat -> True];
    (* FFSkipFrame[st, dim, at-1]; *)
  {st, dim}
];

(*makes a stream*)
FFInputStreamAtNew[file_String, at_Integer, noOfFrames_Integer:1] := 
  Module[{fps, startAtSec, st, dim},
  fps = Import[file, "FrameRate"];
  dim = Import[file, "ImageSize"];
  startAtSec = (at-1) / fps;
  st = OpenRead["!" ~~ ffmpeg ~~ 
    " -i " ~~ file ~~ 
    " -frames:v " ~~ ToString@(at+noOfFrames) ~~
    " -loglevel quiet -f image2pipe -pix_fmt gray -vcodec rawvideo -", 
    BinaryFormat -> True];
  FFSkipFrame[st, dim, at-1];
  {st, dim}
];


(*read one frame*)
FFGetOneFrame[path_String, frame_Integer] := Module[ {st, dim, img},
  {st, dim} = FFInputStreamAt[path, frame, 1];
  img = FFGetNextFrame[st, dim];
  Close[st];
  img
]

(*read multiple frames*)
FFGetOneFrame[path_String, frames_List] := Module[ {order, st, dim, res, ReadFrames},
  order = Sort @ frames;
  {st, dim} = FFInputStreamAt[path, First@order, Last@order - First@order+1]; 
  ReadFrames[] := Reap@Do[
    If[ MemberQ[order,f],
      Sow @ FFGetNextFrame[st, dim] ,
      FFSkipFrame[st, dim]
    ]
    , {f, First@order, Last@order}
  ];
  res = Check[ ReadFrames[], 
                Close[st]; 
                Print@"Failed loading frames. try again."; 
                Return@FFGetOneFrame[path, frames]];
  Close[st];
  res[[2, 1]] (*extract result from reap*)
]

(*read multiple frames - experimental*)
FFGetOneFrameNew[path_String, frames_List] := Module[ {order, st,  dim, res},
  order = Sort @ frames;
  {st, dim} = FFInputStreamAtNew[path, First@order, Last@order - First@order+1];
  res = Reap@Do[
    If[ MemberQ[order,f],
      Sow @ FFGetNextFrame[st, dim] ,
      FFGetNextFrame[st, dim]
    ]
    , {f, First@order, Last@order}
  ];
  Print@"experimental frame grabber - ffmpeg";
  Close[st];
  res[[2, 1]] (*extract result from reap*)
];


(*read multiple frames*)
FFGetAudioSegment[path_String, frames_List] := Module[ {order, st, dim, res, ReadFrames},
  order = Sort @ frames;
  {st, dim} = FFInputStreamAt[path, First@order, Last@order - First@order+1]; 
  ReadFrames[] := Reap@Do[
    If[ MemberQ[order,f],
      Sow @ FFGetNextFrame[st, dim] ,
      FFSkipFrame[st, dim]
    ]
    , {f, First@order, Last@order}
  ];
  res = Check[ ReadFrames[], 
                Close[st]; 
                Print@"Failed loading frames. try again."; 
                Return@FFGetOneFrame[path, frames]];
  Close[st];
  res[[2, 1]] (*extract result from reap*)
]


(* ::Subsection:: *)
(*FFprobe Function/Implementation*)


(*set the path to ffmpeg*)
FFprobe[path_String] := (ffprobe = path;);


(*returns status of ffmpeg.*)
(*todo: on windows it responds, but does not checkout.*)
FFprobe[] := 
  If[ !StringQ@ffprobe,
    Print @ "The path to ffprobe is unknown. Use FFprobe[\"path\"] to set it.",
    (*second option - test if working*)
    If[ StringMatchQ[ 
        ToString @ ReadLine @ OpenRead["!" ~~ ffprobe ~~ " -version", BinaryFormat -> True],
        "ffprobe version*"],
      Print @ "ffprobe was found and is functional",
      Print @ ("ffprobe does not respond correctly. Please check the path: " <> ToString@ffprobe) 
    ]
  ];

(*run on loading - default path*)
Switch[ $OperatingSystem, 
  "MacOSX",  FFprobe @ "/usr/local/bin/ffprobe",
  "Windows", FFprobe @ "ffprobe.exe",
  "Linux",   FFprobe @ "ffprobe"];


FFProbe[file_String, streamCodec_String, targetVariable_String] := 
		FFProbe[file, streamCodec, {targetVariable}][[1]];


FFProbe[file_String, streamCodec_String, {targetVariables__String}] := 
Module[ {tempFile, tempOutput},
  tempFile = FileNameJoin[{$TemporaryDirectory,"tempffprobe.json"}];
  Run[ffprobe <>
		" -print_format json -show_format -show_streams \"" <> 
	    file <> 
		"\" > \"" <> 
		tempFile <>
		"\""
	];
	tempOutput=Import[tempFile];
	tempOutput = Cases[("streams" /. tempOutput),{___, "codec_type"->streamCodec, ___}];
	If[ Length[tempOutput] >= 1,
		ToExpression[{targetVariables} /. tempOutput[[1]]],
		Message[FFGetFrameRate::noStream, streamCodec, file]; {}
	]
];

FFProbe::noStream = "Could not find `1` stream in file `2`.";


FFGetFrameRate[file_String] := FFProbe[file, "video", "r_frame_rate"];


FFGetDuration[file_String] := FFProbe[file, "video", "duration"];


FFGetImageSize[file_String] := FFProbe[file, "video", {"width","height"}];


(* ::Subsection:: *)
(*FFImport Function (putting everything together)*)


(* Importing function*)
FFImport[path_String, elements_] := Switch[ elements, 
  {"Frames", _Integer}, FFGetOneFrame[ path, elements[[2]] ],
  {"Frames", _List}, FFGetOneFrame[path, elements[[2]] ],
  {"Frames", _List, True}, FFGetOneFrameNew[path, elements[[2]] ], (*experimental*)
  "FrameRate", FFGetFrameRate[path],
  "ImageSize", FFGetImageSize[path],
  "Duration", FFGetDuration[path],
  _, Import[path, elements]
];

FFExport[path_String, expr_] := Print @ "not implemented";


(* ::Section:: *)
(*Package Close*)


End[ ]

EndPackage[ ]
