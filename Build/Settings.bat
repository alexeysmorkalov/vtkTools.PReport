@echo off
rem Шаблон для имен файлов дистрибутива
rem параметр - трехзначный номер версии
set d4=PReport%prversion%D4
set d5=PReport%prversion%D5
set d6=PReport%prversion%D6
set d7=PReport%prversion%D7
set bcb5=PReport%prversion%BCB5
set bcb6=PReport%prversion%BCB6
set docrus=prdoc%prversion%rus
set doceng=prdoc%prversion%eng
set src=PReport%prversion%SRC

set prcat=%CD%\..

rem Каталог, с htmp хэлпом для PReport
set prhtml=%prcat%\..\PReport_documentation\Draft_Html

rem Путь к Диминой программе для компиляции хэлпа
set dipas=%prcat%\..\Bin\dipas_console_m.exe

rem Каталог, куда помещать дистрибутив
set dpup=%prcat%\..\..\upload

rem Временный каталог для сборки дистрибутива
set dppr=%dpup%\Temp
set dplib=%dpup%\TempLib

rem Упаковщик 
set wz=%CD%\..\..\bin\pkzip25.exe

set prrescompile=%prcat%\RES\ENG

set commoncontrols=%prcat%\..\CommonControls\Source

set d4cat=C:\Program Files\Borland\Delphi4
set d5cat=D:\Program Files\Borland\Delphi5
set d6cat=D:\Program Files\Borland\Delphi6
set d7cat=D:\Program Files\Borland\Delphi7
set bcb5cat=D:\Program Files\Borland\CBuilder5
set bcb6cat=D:\Program Files\Borland\CBuilder6
