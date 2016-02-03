Dim WshShell
Dim obj
Set WshShell = WScript.CreateObject("WScript.Shell")
obj = WshShell.Run("start.bat", 0)
set WshShell = Nothing
