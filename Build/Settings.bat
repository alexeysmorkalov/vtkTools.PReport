@echo off
rem ������ ��� ���� 䠩��� ����ਡ�⨢�
rem ��ࠬ��� - ��姭��� ����� ���ᨨ
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

rem ��⠫��, � htmp ���� ��� PReport
set prhtml=%prcat%\..\PReport_documentation\Draft_Html

rem ���� � ������� �ணࠬ�� ��� �������樨 ���
set dipas=%prcat%\..\Bin\dipas_console_m.exe

rem ��⠫��, �㤠 ������� ����ਡ�⨢
set dpup=%prcat%\..\..\upload

rem �६���� ��⠫�� ��� ᡮન ����ਡ�⨢�
set dppr=%dpup%\Temp
set dplib=%dpup%\TempLib

rem ������騪 
set wz=%CD%\..\..\bin\pkzip25.exe

set prrescompile=%prcat%\RES\ENG

set commoncontrols=%prcat%\..\CommonControls\Source

set d4cat=C:\Program Files\Borland\Delphi4
set d5cat=D:\Program Files\Borland\Delphi5
set d6cat=D:\Program Files\Borland\Delphi6
set d7cat=D:\Program Files\Borland\Delphi7
set bcb5cat=D:\Program Files\Borland\CBuilder5
set bcb6cat=D:\Program Files\Borland\CBuilder6
