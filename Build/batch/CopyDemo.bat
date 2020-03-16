rem *********************************************
rem Usage:
rem   CopyDemo.bat <директория источник>, <директория приемник>
rem *********************************************

set demosrc=%1
set demodst=%2

if exist %demosrc%\nul (
mkdir %demodst%

copy %demosrc%\*.* %demodst%

if exist %demosrc%\cbuilder\nul (
mkdir %demodst%\cbuilder
copy %demosrc%\cbuilder\*.cpp %demodst%\cbuilder
copy %demosrc%\cbuilder\*.dfm %demodst%\cbuilder
copy %demosrc%\cbuilder\*.res %demodst%\cbuilder
copy %demosrc%\cbuilder\*.h   %demodst%\cbuilder
copy %demosrc%\cbuilder\*.ini %demodst%\cbuilder
copy %demosrc%\cbuilder\*.bpr %demodst%\cbuilder
copy %demosrc%\cbuilder\*.prt %demodst%\cbuilder
copy %demosrc%\cbuilder\*.dbf %demodst%\cbuilder
copy %demosrc%\cbuilder\readme.txt %demodst%\cbuilder
if exist %demosrc%\cbuilder\Reports\nul (
mkdir %demodst%\cbuilder\Reports
pause
copy %demosrc%\cbuilder\Reports\*.prt %demodst%\cbuilder\Reports))


if exist %demosrc%\delphi\nul (
mkdir %demodst%\delphi
copy %demosrc%\delphi\*.pas %demodst%\delphi
copy %demosrc%\delphi\*.dfm %demodst%\delphi
copy %demosrc%\delphi\*.res %demodst%\delphi
copy %demosrc%\delphi\*.ini %demodst%\delphi
copy %demosrc%\delphi\*.dpr %demodst%\delphi
copy %demosrc%\delphi\*.prt %demodst%\delphi
copy %demosrc%\delphi\*.dbf %demodst%\delphi
copy %demosrc%\delphi\readme.txt %demodst%\delphi
if exist %demosrc%\delphi\Reports\nul (
mkdir %demodst%\delphi\Reports
copy %demosrc%\delphi\Reports\*.prt %demodst%\delphi\Reports)))

