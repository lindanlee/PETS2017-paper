set /p runid=<%USERPROFILE%\runid.txt
set /p version=<%USERPROFILE%\version.txt
set /p env=<C:\Users\environment.txt
"C:\Program Files (x86)\VideoLAN\VLC\vlc.exe" --one-instance vlc://quit
timeout 20 /nobreak
del /q "%TEMP%\upload"
rmdir "%TEMP%\upload"
mkdir "%TEMP%\upload"
copy "%USERPROFILE%\%env%-%version%-%runid%.mp4" "%TEMP%\upload"
copy "%USERPROFILE%\Desktop\Tor Browser\Browser\inst.log" "%TEMP%\upload\%env%-%version%-%runid%-inst.log"
copy "%USERPROFILE%\Desktop\Tor Browser\Browser\inst.log" "S:\lee\%env%-%version%-%runid%-inst.log"
s3-uploader.exe
