@echo off

set prversion=%1
call Settings.bat

echo *********************************************************
echo
echo (bat file works only on NT/2000/XP):
echo
echo   %d4%.zip
echo   %d5%.zip - PReport without sources
echo   %d6%.zip
echo   %d7%.zip
echo   %bcb5%.zip
echo   %bcb6%.zip
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
md %dppr%\PReport\RES
md %dppr%\PReport\Demo

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
rem Delphi
rem *********************************************

set dcat=%d4cat%
rmdir %dppr%\PReport\Source /q /s
md %dppr%\PReport\Source
call batch\BuildDelphi.bat vcl40
call batch\CopyDelphiDCU.bat %dppr%\PReport\Source 4
%wz% -add -max -dir=relative %dpup%\%d4%.zip %dppr%\*.*

set dcat=%d5cat%
rmdir %dppr%\PReport\Source /q /s
md %dppr%\PReport\Source
call batch\BuildDelphi.bat vcl50
call batch\CopyDelphiDCU.bat %dppr%\PReport\Source 5
%wz% -add -max -dir=relative %dpup%\%d5%.zip %dppr%\*.*

set dcat=%d6cat%
rmdir %dppr%\PReport\Source /q /s
md %dppr%\PReport\Source
call batch\BuildDelphi.bat DesignIde
call batch\CopyDelphiDCU.bat %dppr%\PReport\Source 6
%wz% -add -max -dir=relative %dpup%\%d6%.zip %dppr%\*.*

set dcat=%d7cat%
rmdir %dppr%\PReport\Source /q /s
md %dppr%\PReport\Source
call batch\BuildDelphi.bat DesignIde
call batch\CopyDelphiDCU.bat %dppr%\PReport\Source 7
%wz% -add -max -dir=relative %dpup%\%d7%.zip %dppr%\*.*

rem *********************************************
rem Builder
rem *********************************************

set dcat=%bcb5cat%
rmdir %dppr%\PReport\Source /q /s
md %dppr%\PReport\Source
call batch\BuildBuilder.bat vcl50
call batch\CopyBuilderDCU.bat %dppr%\PReport\Source 5
%wz% -add -max -dir=relative %dpup%\%bcb5%.zip %dppr%\*.*

set dcat=%bcb6cat%
rmdir %dppr%\PReport\Source /q /s
md %dppr%\PReport\Source
call batch\BuildBuilder.bat DesignIde
call batch\CopyBuilderDCU.bat %dppr%\PReport\Source 6
%wz% -add -max -dir=relative %dpup%\%bcb6%.zip %dppr%\*.*

rmdir %dppr% /q /s
