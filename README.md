ffmpeg-mathematica
==================

Mathematica package for using FFMPEG video library. This package accurately  imports video frames into Mathematica. It is necessary for scientific applications as Mathematica's (v9) default methods uses QuickTime that produces artifact in uncompressed videos or duplicated frames. 

## Usage

Mimicking Mathematica's Import function:
`FFImport[ "file.avi", {"Frames", 1}]` - gives first frame (slow way)
`FFImport[ "file.avi", {"Frames", Range[1,1000]}]` - gives first 1000 frames (fast)

To test status of ffmpeg library you can run `FFmpeg[]`.

## Installation

You will need to add package via ```<<FFmpeg` ``` if the package is in the path or via `Import["FFmpeg.m"]`. 

To install ffmpeg:

### MAC

Using homebrew `brew install ffmpeg`

### Windows

[follow these instructions](http://www.wikihow.com/Install-FFmpeg-on-Windows)

### Linux

Untested at the moment. Must be accessible from terminal via command `ffmpeg`. Otherwise path can specified via `FFmpeg["usr/bin/.../ffmpeg"]`.

## ToDo List and known bugs

 - the fast lookup is not implemented. It is slow to go though large videos to the right frame. Load video in big lumps to avoid this penalty.
 - Mathematica's method of reporting number of frames sometimes gives wrong estimates. If asked for frames that do not exist this package will attempt to load it many times and must be aborted. 

## Contributors

 - Karolis Misiunas (github: kmisiunas)

## Help Improving it further!

