ffmpeg-mathematica
==================

Mathematica package for using FFMPEG video library. This package accurately  imports video frames into Mathematica. It is necessary for scientific applications as Mathematica's (v9 or v10) default methods uses QuickTime that produces artifact in uncompressed videos or duplicated frames. It also is useful for importing other video-codecs.

## Usage

Mimicking Mathematica's Import function:  
`FFImport[ "file.avi", {"Frames", 1}]` - gives first frame (slow way)  
`FFImport[ "file.avi", {"Frames", Range[1,1000]}]` - gives first 1000 frames (fast)

Also supports some property look-up functions using ffprobe:  
`FFImport[ "file.avi", "FrameRate"]` - returns the frame rate of the video  
`FFImport[ "file.avi", "Duration"]` - returns the duration of the video in sec
`FFImport[ "file.avi", "ImageSize"]` - frame size in pixels  


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

## Advanced Usage

### Load whole video FAST

If you are aiming for the best performance the package provides special tools for optimising your program. I achieved best performance when loading entire video with a single ffmpeg stream. In such case there is no need to go though the file multiple times. Three special functions are supplied: open stream with `FFInputStreamAt`, get frame with `FFGetNextFrame` or skip one `FSkipFrame`. See function docs for more info `?FFInputStreamAt`. And don't forget to close the stream with `Close[st]` at the end. 

### Color Mode

Color mode can be changed via settings. Default one is "rgb24". If grey-scale is required, use:
`Options[FFmpeg] = {
   "Colors" -> 1 (*number of color channels*), 
   "ColorCommand" -> "gray" (*indicator for "-pix_fmt" parameter:gray/
   rgb24*)
   };`

## ToDo List and known bugs

 - There is sometimes an error loading the video - where the ffmpeg reports end of stream before entire video was loaded. On the internet there seems to be a suggestion that stream is not closed properly in Mathematica. Be careful! In my experience it is a problem with external USB hard-drives with MAC OS X.
 - the fast lookup is not implemented. It is slow to go though large videos to the right frame. Load video in big lumps to avoid this penalty. 

## ToDo

 - Restructure to work with built in Import[] function.

## Contributors

 - Karolis Misiunas (github: kmisiunas)
 - Kenta Takagaki (github: ktakagaki)

## Help Improving it further!

