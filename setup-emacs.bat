
@echo off
echo thesaitama Emacs setup for Windows

set /p CONFIRM="copy emacs configuration (y/n):"
if %CONFIRM% == n goto END

cd /d %~dp0
xcopy /H /Y .emacs.el %APPDATA%\.emacs.el

if not exist %APPDATA%\dotfiles ( mkdir "%APPDATA%\dotfiles\" )
if not exist %APPDATA%\dotfiles\elisp ( mkdir "%APPDATA%\dotfiles\elisp\" )
if not exist %APPDATA%\dotfiles\elisp\ext ( mkdir "%APPDATA%\dotfiles\elisp\ext\" )
if not exist %APPDATA%\dotfiles\elisp\legacy ( mkdir "%APPDATA%\dotfiles\elisp\legacy\" )

xcopy /H /Y elisp\* %APPDATA%\dotfiles\elisp
xcopy /H /Y elisp\ext\* %APPDATA%\dotfiles\elisp\ext
xcopy /H /Y elisp\legacy\* %APPDATA%\dotfiles\elisp\legacy

pause

:END
exit