rem *********************************************
rem Usage:
rem   BuildBuilder.bat <ЗначениеДляКлючаLU>
rem Должна быть установлена переменная dcat, на
rem каталог с нужной версией  Builder
rem *********************************************

set lukey=%1

rem Directory with sources
set inc=%prcat%\Source

rem UNITS directories
set units=%dcat%\Lib;%dcat%\Projects\Bpl;%commoncontrols%;%prcat%\Source

rem Directory with resources
set res=%prcat%\RES

rem Directory with compiler
set cmp=%dcat%\Bin

rem Delete files
del %prcat%\Source\*.dcu
del %prcat%\Source\*.obj
del %prcat%\Source\*.hpp

for %%f in (%prcat%\Source\*.pas) do "%cmp%\dcc32.exe" -DVTK_NOLIC -LU%lukey% -$D- -$L- -$Y- -JPHNE -B -R%res% -I"%inc%" -U"%units%" "%%f"
