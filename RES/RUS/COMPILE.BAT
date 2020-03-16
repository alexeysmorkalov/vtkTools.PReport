brcc32.exe prStrings.rc
brcc32.exe prDialogs.rc

copy TxRO.ini ..
brcc32.exe ..\prTxRO.rc

copy prStrings.res ..
copy prDialogs.res ..
copy prImages.res ..
del prStrings.res
del prDialogs.res
