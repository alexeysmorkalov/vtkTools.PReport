@echo off

set prversion=%1
call Settings.bat

echo *********************************************************
echo
echo (bat file works only on NT/2000/XP):
echo
echo   %src%.zip - PReport sources (Source and Res)
echo
echo *********************************************************
pause

md %dpup%
rmdir %dppr% /s /q
md %dppr%

rem *********************************************
rem Формируем каталог
rem *********************************************
md %dppr%\CommonControls
md %dppr%\CommonControls\Source
md %dppr%\PReport
md %dppr%\PReport\Source
md %dppr%\PReport\RES
md %dppr%\PReport\Demo

rem *********************************************
rem Sources
rem *********************************************
copy %prcat%\source\*.pas %dppr%\PReport\Source
copy %prcat%\source\*.res %dppr%\PReport\Source
copy %prcat%\source\*.bpk %dppr%\PReport\Source
copy %prcat%\source\*.dfm %dppr%\PReport\Source
copy %prcat%\source\*.inc %dppr%\PReport\Source
copy %prcat%\source\*.dpk %dppr%\PReport\Source
copy %prcat%\source\*.cpp %dppr%\PReport\Source
copy %prcat%\source\*.inc %dppr%\PReport\Source

rem *********************************************
rem Resources
rem *********************************************
call batch\CopyRes.bat

rem *********************************************
rem Demos
rem *********************************************
call batch\CopyDemos.bat

rem *********************************************
rem Addititional files
rem *********************************************
copy %prcat%\..\PReport_documentation\readme.txt %dppr%
copy %prcat%\..\PReport_documentation\PReportEULA.txt %dppr%

rem *********************************************
rem CommonControls
rem *********************************************
copy %commoncontrols%\*.pas %dppr%\CommonControls\Source
copy %commoncontrols%\*.dfm %dppr%\CommonControls\Source
copy %commoncontrols%\*.dpk %dppr%\CommonControls\Source
copy %commoncontrols%\*.res %dppr%\CommonControls\Source
copy %commoncontrols%\*.bpk %dppr%\CommonControls\Source
copy %commoncontrols%\*.cpp %dppr%\CommonControls\Source
copy %commoncontrols%\*.inc %dppr%\CommonControls\Source

rem *********************************************
rem Создаем архив
rem *********************************************

%wz% -add -max -dir=relative %dpup%\%src%.zip %dppr%\*.*

rmdir %dppr% /q /s
