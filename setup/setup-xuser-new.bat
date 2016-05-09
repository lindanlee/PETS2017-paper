"R:\lee\torbrowser-new-5.0.3_en-US.exe" /S
echo NEW>%USERPROFILE%\version.txt
set "runid=%ComputerName%-%DATE:~10,4%%DATE:~4,2%%DATE:~7,2%-%TIME:~0,2%%TIME:~3,2%%TIME:~6,2%"
echo %runid%>%USERPROFILE%\runid.txt
set /p version=<%USERPROFILE%\version.txt
set /p env=<C:\Users\environment.txt
start "" "C:\Program Files (x86)\VideoLAN\VLC\vlc.exe" screen:// --sout="#transcode{vcodec=h264,acodec=mpga,ab=128,channels=2,samplerate=44100}:file{dst=%USERPROFILE%\%env%-%version%-%runid%.mp4,no-overwrite}" --sout-keep --screen-fps 10 --live-caching 300 --qt-start-minimized --one-instance
