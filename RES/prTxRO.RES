        ��  ��                  F  (   ��
 T X R O         0         [General]
DefaultRecodeTable=

[CommonBorderSchemas]
BorderSchemasCount=1
BorderScheme1=Standard single,#124#43#45#43#124#43#45#43

[RecodeTable_Turkish_Auto_857_to_1254]
Description=Turkish 857(OEM) to 1254(ANSI)
OEMtoWINTable=#199#252#233#226#228#224#229#231#234#235#232#239#238#253#196#197#201#230#198#244#246#242#251#249#221#214#220#248#163#216#222#254#225#237#243#250#241#209#208#240#191#174#172#189#188#161#171#187#128#129#130#131#132#193#194#192#169#133#134#135#136#162#165#137#138#139#140#141#142#143#227#195#144#145#146#147#148#149#150#164#186#170#202#203#200#151#205#206#207#152#153#154#155#166#204#156#211#223#212#210#245#213#181#157#215#218#219#217#236#255#175#180#173#177#158#190#182#167#247#184#176#168#183#185#179#178#159#160

[RecodeTable_Greece_Auto_737_to_1253]
Description=Greece 737(OEM) to 1253(ANSI)
OEMtoWINTable=#193#194#195#196#197#198#199#200#201#202#203#204#205#206#207#208#209#211#212#213#214#215#216#217#225#226#227#228#229#230#231#232#233#234#235#236#237#238#239#240#241#243#242#244#245#246#247#248#128#129#130#131#132#133#134#135#136#137#138#139#140#141#142#143#144#145#146#147#148#149#150#151#152#153#154#155#156#157#158#159#161#163#164#165#166#167#168#169#170#171#172#173#174#175#179#180#249#220#221#222#250#223#252#253#251#254#162#184#185#186#188#190#191#177#181#182#218#219#187#189#176#192#183#210#224#178#255#160

[RecodeTable_German_Auto_850_to_1252]
Description=German 850(OEM) to 1252(ANSI)
OEMtoWINTable=#199#252#233#226#228#224#229#231#234#235#232#239#238#236#196#197#201#230#198#244#246#242#251#249#255#214#220#248#163#216#215#131#225#237#243#250#241#209#170#186#191#174#172#189#188#161#171#187#128#129#130#132#133#193#194#192#169#134#135#136#137#162#165#138#139#140#141#142#143#144#227#195#145#146#147#148#149#150#151#164#240#208#202#203#200#152#205#206#207#153#154#155#156#166#204#157#211#223#212#210#245#213#181#254#222#218#219#217#253#221#175#180#173#177#158#190#182#167#247#184#176#168#183#185#179#178#159#160

[RecodeTable_Russian866and1251]
Description=Russian DOS866 and Windows 1251
OEMtoWINTable=#192#193#194#195#196#197#198#199#200#201#202#203#204#205#206#207#208#209#210#211#212#213#214#215#216#217#218#219#220#221#222#223#224#225#226#227#228#229#230#231#232#233#234#235#236#237#238#239#128#129#130#131#132#133#134#135#136#137#138#139#140#141#142#143#144#145#146#147#148#149#150#151#152#153#154#155#156#157#158#159#160#161#162#163#164#165#166#167#168#169#170#171#172#173#174#175#240#241#242#243#244#245#246#247#248#249#250#251#252#253#254#255#176#177#178#179#180#181#182#183#184#185#186#187#185#189#190#191
BorderSchemasCount=7
BorderScheme1=Single,#179#218#196#191#179#217#196#192
BorderScheme2=Double,#186#201#205#187#186#188#205#200
BorderScheme3=Single-Double,#179#218#196#183#186#188#205#212
BorderScheme4=Double-Single,#186#201#205#184#179#217#196#211
BorderScheme5=Solid light,#176#176#176#176#176#176#176#176
BorderScheme6=Solid,#177#177#177#177#177#177#177#177
BorderScheme7=Solid dark,#178#178#178#178#178#178#178#178

[TxCommands]
; !!! Dont remove these commands !!!
txcReset = @ Reset priter settings to default
txcFormFeed = = Form feed
txcNormal = D Normal font
txcPica = P Pica font
txcElite = E Elite font
txcCondensedOn = C Condensed on
txcCondensedOff = c Condensed off
txcBoldOn = B Bold on
txcBoldOff = b Bold off
txcItalicOn = I Italic on
txcItalicOff = i Italic off
txcUnderlineOn = U Underline on
txcUnderlineOff = u Underline off
txcSuperScriptOn = H Super script on
txcSuperScriptOff = h Super script off
txcSubScriptOn = L Sub script on
txcSubScriptOff = l Sub script off
txcWideOn = W Wide on
txcWideOff = w Wide off
txc12cpiOn = Q 12cpi on
txc12cpiOff = q 12cpi off
; custom commands
txcLandscape = R Landscape orientation
txcPortrait = r Portrait orientation

[TxFontStyles]
; !!! Dont remove !!!
tfsNormal = txcNormal,Normal
tfsPica = txcPica,Pica
tfsElite = txcElite,Elite

[TxFontOptions]
; !!! Dont remove !!!
tfoCondensed = txcCondensedOn,txcCondensedOff,Condensed
tfoBold = txcBoldOn,txcBoldOff,Bold
tfoItalic = txcItalicOn,txcItalicOff,Italic
tfoUnderline = txcUnderlineOn,txcUnderlineOff,Underline
tfoSuperScript = txcSuperScriptOn,txcSuperScriptOff,Super script
tfoSubScript = txcSubScriptOn,txcSubScriptOff,Sub script
tfoWide = txcWideOn,txcWideOff,Wide
tfo12cpi = txc12cpiOn,txc12cpiOff,12 cpi

[ESCModel_Epson]
ModelName=Epson printers
PrinterDriver=epson
txcFormFeed=#12
txcNormal=#27#120#00
txcPica=#27#120#01#27#107#00
txcElite=#27#120#01#27#107#01
txcCondensedOn=#15
txcCondensedOff=#18
txcBoldOn=#27#71
txcBoldOff=#27#72
txcItalicOn=#27#52
txcItalicOff=#27#53
txcUnderlineOn=#27#45#49
txcUnderlineOff=#27#45#48
txcSuperScriptOn=#27#83#01
txcSuperScriptOff=#27#84
txcSubScriptOn=#27#83#00
txcSubScriptOff=#27#84
txcWideOn=#27#87#01
txcWideOff=#27#87#00
txc12cpiOn=#27#77
txc12cpiOff=#27#80

[ESCModel_HP]
ModelName=HP printers
PrinterDriver=HP
txcFormFeed=#12
txcNormal=
txcPica=
txcElite=
txcCondensedOn=#27#38#107#50#83
txcCondensedOff=#27#38#107#48#83
txcBoldOn=#27#40#115#51#66
txcBoldOff=#27#40#115#48#66
txcItalicOn=#27#40#115#49#83
txcItalicOff= #27#40#115#48#83
txcUnderlineOn=#27#38#100#48#68
txcUnderlineOff=#27#38#100#64
txcSuperScriptOn=
txcSuperScriptOff =
txcSubScriptOn=
txcSubScriptOff =
txcWideOn=
txcWideOff =
txc12cpiOn =
txc12cpiOff =
txcLandscape=#027#038#108#049#079
txcPortrait=#027#038#108#048#079
  