rem *********************************************
rem Usage:
rem   CopyResDir.bat <директория источник>, <директория приемник>
rem *********************************************

set ressrc=%1
set resdst=%2

mkdir %resdst%

copy %ressrc%\*.rc %resdst%
copy %ressrc%\*.res %resdst%
copy %ressrc%\*.bat %resdst%
copy %ressrc%\*.ini %resdst%
