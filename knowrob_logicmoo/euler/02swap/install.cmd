@ echo off
if not exist "C:\Program Files\eye\src" mkdir "C:\Program Files\eye\src"
copy /Y "%~dp0euler.yap" "C:\Program Files\eye\src"
if not exist "C:\Program Files\eye\lib" mkdir "C:\Program Files\eye\lib"
pushd "C:\Program Files\eye\lib"
swipl -q -f ../src/euler.yap -g main -- --image eye.pvm
popd
if not exist "C:\Program Files\eye\bin" mkdir "C:\Program Files\eye\bin"
copy /Y "%~dp0eye.cmd" "C:\Program Files\eye\bin"
