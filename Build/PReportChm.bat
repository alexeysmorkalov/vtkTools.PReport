@echo off

call Settings.bat

rem *********************************************
rem Recreate temporary directory
rem *********************************************
rmdir %dppr% /s /q

rem *********************************************
rem Copy addititional HTML files
rem *********************************************
md %dppr%\Images
md %dppr%\Templates
copy %prhtml%\*.* %dppr%
copy %prhtml%\Images\*.* %dppr%\Images
copy %prhtml%\Templates\*.* %dppr%\Templates

rem *********************************************
rem Build CHM
rem *********************************************
set curd=%CD%
cd %dppr%
start /wait %dipas% -OHtmlHelp -I%prcat%\Source %prcat%\Source\*.pas -Le -XHomePage -Mdefaultvisibility+ -Mprivate- -Mprotected- -NPReport -F%prhtml%\footer -H%prhtml%\header -Askipempty
cd %curd%

rem *********************************************
rem Copy CHM to destination directory
rem *********************************************

@echo Подождите пока закроются все всплывающие окна и нажмите любую клавишу
pause
copy %dppr%\PReport.chm %dpup%
move %dppr%\dipas_console_m.log %dpup%\PReport_dipas_console_m.log

%dppr% /s /q
