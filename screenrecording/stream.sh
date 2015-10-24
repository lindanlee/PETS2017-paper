#!/bin/sh

set -e

OUTFILENAME=$HOME/ux/capture-$(date +'%Y%m%d-%H:%M:%S').mp4
echo "Saving to $OUTFILENAME"

/Applications/VLC.app/Contents/MacOS/VLC -I dummy screen:// --sout="#transcode{vcodec=h264}:duplicate{dst=file{dst=$OUTFILENAME,no-overwrite},dst=http{mux=ffmpeg{mux=flv},dst=:8080/}}" --sout-keep --screen-fps 10 --live-caching 300
