rem *********************************************
rem Usage:
rem   CopyDemos.bat
rem *********************************************

rem *********************************************
rem Data tables
rem *********************************************
md %dppr%\PReport\Demo\DBF
copy %prcat%\Demo\DBF\*.* %dppr%\PReport\Demo\DBF

rem *********************************************
rem Demos
rem *********************************************
call batch\CopyDemo.bat %prcat%\Demo\Win %dppr%\PReport\Demo\Win
call batch\CopyDemo.bat %prcat%\Demo\Dos %dppr%\PReport\Demo\Dos
call batch\CopyDemo.bat %prcat%\Demo\UserActionsInPreview %dppr%\PReport\Demo\UserActionsInPreview


call batch\CopyDemo.bat %prcat%\Demo\Demos\01_SimpleDemo %dppr%\PReport\Demo\Demos\01_SimpleDemo
call batch\CopyDemo.bat %prcat%\Demo\Demos\02_MulticolumnDemo %dppr%\PReport\Demo\Demos\02_MulticolumnDemo
call batch\CopyDemo.bat %prcat%\Demo\Demos\03_GroupsDemo %dppr%\PReport\Demo\Demos\03_GroupsDemo
call batch\CopyDemo.bat %prcat%\Demo\Demos\04_VarsDemo %dppr%\PReport\Demo\Demos\04_VarsDemo
call batch\CopyDemo.bat %prcat%\Demo\Demos\05_SimpleCrossTab %dppr%\PReport\Demo\Demos\05_SimpleCrossTab
call batch\CopyDemo.bat %prcat%\Demo\Demos\06_AdvancedCrossTab %dppr%\PReport\Demo\Demos\06_AdvancedCrossTab
call batch\CopyDemo.bat %prcat%\Demo\Demos\07_DynamicResize %dppr%\PReport\Demo\Demos\07_DynamicResize
call batch\CopyDemo.bat %prcat%\Demo\Demos\08_UserVars %dppr%\PReport\Demo\Demos\08_UserVars
call batch\CopyDemo.bat %prcat%\Demo\Demos\09_MDI %dppr%\PReport\Demo\Demos\09_MDI
call batch\CopyDemo.bat %prcat%\Demo\Demos\10_DLL %dppr%\PReport\Demo\Demos\10_DLL
call batch\CopyDemo.bat %prcat%\Demo\Demos\11_ReportCreatedAtRuntime %dppr%\PReport\Demo\Demos\11_ReportCreatedAtRuntime
call batch\CopyDemo.bat %prcat%\Demo\Demos\12_Drag-Drop %dppr%\PReport\Demo\Demos\12_Drag-Drop
call batch\CopyDemo.bat %prcat%\Demo\Demos\13_ChangingAtRuntime %dppr%\PReport\Demo\Demos\13_ChangingAtRuntime
call batch\CopyDemo.bat %prcat%\Demo\Demos\14_NewObjectsLinksModes %dppr%\PReport\Demo\Demos\14_NewObjectsLinksModes
call batch\CopyDemo.bat %prcat%\Demo\Demos\15_GroupedTprDataset %dppr%\PReport\Demo\Demos\15_GroupedTprDataset
call batch\CopyDemo.bat %prcat%\Demo\Demos\16_ChangePagesNumbering %dppr%\PReport\Demo\Demos\16_ChangePagesNumbering
call batch\CopyDemo.bat %prcat%\Demo\Demos\17_TextReportsWithVariousLength %dppr%\PReport\Demo\Demos\17_TextReportsWithVariousLength
call batch\CopyDemo.bat %prcat%\Demo\Demos\18_RotatedReport %dppr%\PReport\Demo\Demos\18_RotatedReport
call batch\CopyDemo.bat %prcat%\Demo\Demos\19_CardReport %dppr%\PReport\Demo\Demos\19_CardReport
call batch\CopyDemo.bat %prcat%\Demo\Demos\20_Split %dppr%\PReport\Demo\Demos\20_Split
call batch\CopyDemo.bat %prcat%\Demo\Demos\21_ImagesFromFile %dppr%\PReport\Demo\Demos\21_ImagesFromFile
call batch\CopyDemo.bat %prcat%\Demo\Demos\22_PrintVariables %dppr%\PReport\Demo\Demos\22_PrintVariables
call batch\CopyDemo.bat %prcat%\Demo\Demos\23_CustomSaveOpenInDesigner %dppr%\PReport\Demo\Demos\23_CustomSaveOpenInDesigner
call batch\CopyDemo.bat %prcat%\Demo\Demos\24_AppendPreparedReport %dppr%\PReport\Demo\Demos\24_AppendPreparedReport
call batch\CopyDemo.bat %prcat%\Demo\Demos\25_UsingParser %dppr%\PReport\Demo\Demos\25_UsingParser
call batch\CopyDemo.bat %prcat%\Demo\Demos\26_SubReports %dppr%\PReport\Demo\Demos\26_SubReports
call batch\CopyDemo.bat %prcat%\Demo\Demos\27_RichEdit %dppr%\PReport\Demo\Demos\27_RichEdit
call batch\CopyDemo.bat %prcat%\Demo\Demos\28_RtfSplit %dppr%\PReport\Demo\Demos\28_RtfSplit
call batch\CopyDemo.bat %prcat%\Demo\Demos\29_JPEGImages %dppr%\PReport\Demo\Demos\29_JPEGImages
call batch\CopyDemo.bat %prcat%\Demo\Demos\30_BarCodes %dppr%\PReport\Demo\Demos\30_BarCodes
call batch\CopyDemo.bat %prcat%\Demo\Demos\31_AccessToAggVariables %dppr%\PReport\Demo\Demos\31_AccessToAggVariables
call batch\CopyDemo.bat %prcat%\Demo\Demos\32_PrintStringListAsRTF %dppr%\PReport\Demo\Demos\32_PrintStringListAsRTF
