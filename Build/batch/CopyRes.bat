rem *********************************************
rem Usage:
rem   CopyRes.bat
rem *********************************************

call %prcat%\RES\ENG\compile.bat
copy %prcat%\RES\*.res %dppr%\PReport\RES
copy %prcat%\RES\*.ini %dppr%\PReport\RES
copy %prcat%\RES\*.rc %dppr%\PReport\RES

call batch\CopyResDir.bat %prcat%\RES\ENG %dppr%\PReport\RES\ENG
call batch\CopyResDir.bat %prcat%\RES\RUS %dppr%\PReport\RES\RUS
