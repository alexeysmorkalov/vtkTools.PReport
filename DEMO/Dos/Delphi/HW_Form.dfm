�
 THWFORM 0�)  TPF0THWFormHWFormLeftITopdBorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionDOS demoClientHeight6ClientWidth�Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderOnClose	FormCloseOnCreate
FormCreatePixelsPerInch`
TextHeight TSpeedButtonSpeedButton1Left`TopWidth9HeightCaptionDesignOnClickbDesignClick  TSpeedButtonSpeedButton2Left�TopWidth9HeightCaptionPreviewOnClickbPreviewClick  TSpeedButtonSpeedButton3Left�TopWidth)HeightCaptionNewOnClickToolbarButton971Click  TSpeedButtonSpeedButton4Left�TopWidthiHeightCaptionClear prepared dataOnClickSpeedButton4Click  TSpeedButtonSpeedButton5Left`TopWidth)HeightCaptionFreeOnClickSpeedButton5Click  TLabelLabel1Left� Top WidthHeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFont  	TComboBoxCBTemplatesLeftTopWidthQHeightStylecsOwnerDrawFixedDropDownCount
ItemHeightTabOrder 
OnDrawItemCBTemplatesDrawItem  	TCheckBoxCBShowProgressLeftTop Width� HeightCaptionShow progress while generateChecked	State	cbCheckedTabOrder  
TPopupMenu	PMWindows  
TImageList
ImageList1Left Bitmap
&  IL     �������������BM6       6   (   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ���         ��� ��� ��� ��� ��� ��� ��� ��� ��� ���                                                                                                                                                             ���     ���     ���     ���     ���     ���     ���         ��� ��� ��� ��� ��� ��� ���                                                                                                                                                                                                                                     ��� ��� ��� ��� ��� ���                                                                                                                                                                                 ��  ��  ��  ��  ��  ��  ��  ��                          ��� ��� ��� ��� ���                 ��                                                                                                                                                                  ��                  ��  ��                              ��� ��� ��� ��� ���                                                                                                                                                                                     ��      ���     ��  ��                                  ��� ��� ��� ��� ���         ��                                                                                                                                                                          ��          ��  ��                                      ��� ��� ��� ��� ���         ��  ��                                                                                                                                                                      ��      ��  ��                                          ��� ��� ��� ��� ��� ���                                                                                                                                                                                 ��  ��  ��                       �� ���                 ��� ��� ��� ��� ��� ��� ���                                                                                                                                                                             ��  ��                           �� ���                 ��� ��� ��� ��� ��� ��� ��� ��� ��� ���                                                                                                                                                                 ��                                   �� ���             ��� ��� ��� ��� ��� ��� ���                                                                                                                                                                                                                  ��                 ��� ��� ��� ��� ��� ��� ���                                                                                                                                                                                                                       �   �         ��� ��� ��� ��� ��� ��� ���                                                                                                                                                                                                                                                                                                                                                                                                                                 BM>       >   (   @            �                       ��� ����    �      �      �  	    �  �    ��    ��    �{    �O;    �� �    ��     ��     ��     �� _    �� ?    ��                             TTableLookupsPartsDatabaseNameDatabase	TableNameparts.dbLeft�Top  TTableItemsLinkedToOrdersDatabaseNameDatabase	TableNameitems.dbLeftpTop TFloatFieldItemsLinkedToOrdersOrderNo	FieldNameOrderNoDisplayFormat'#'0000  TFloatFieldItemsLinkedToOrdersItemNo	FieldNameItemNo  TFloatFieldItemsLinkedToOrdersPartNo	AlignmenttaLeftJustify	FieldNamePartNoDisplayFormatPN-00000  TIntegerFieldItemsLinkedToOrdersQty	FieldNameQty  TFloatFieldItemsLinkedToOrdersDiscount	FieldNameDiscountDisplayFormat#%MaxValue       �@  TCurrencyFieldItemsLinkedToOrdersListPrice	FieldKindfkLookup	FieldName	ListPriceLookupDataSetLookupsPartsLookupKeyFieldsPartNoLookupResultField	ListPrice	KeyFieldsPartNoLookup	  TStringFieldItemsLinkedToOrdersDescriptionDisplayWidth	FieldKindfkLookup	FieldNameDescriptionLookupDataSetLookupsPartsLookupKeyFieldsPartNoLookupResultFieldDescription	KeyFieldsPartNoSizeLookup	   TTableOrdersLinkedToCustomersAfterScroll"OrdersLinkedToCustomersAfterScrollDatabaseNameDatabase	IndexNameCustNo	TableName	orders.dbLeftPTop  TTable	customersAfterScrollcustomersAfterScrollDatabaseNameDatabase	TableNamecustomer.dbLeft� Top  TTableordersDatabaseNameDatabase	TableName	orders.dbLeft� Top  TTablepartsDatabaseNameDatabase	TableNameparts.dbLeft� Top  TTableTable1DatabaseNameDatabase	TableName
TABLE1.DBFLeft�Top  TTableTable2DatabaseNameDatabase	TableName
TABLE2.DBFLeft�Top  TTablebiolifeDatabaseNameDatabase	TableName
biolife.dbLeftTop  TTableCustomersByNameDatabaseNameDatabase	IndexName	ByCompany	TableNamecustomer.dbLeft0Top  TQueryQ_SPOSTDatabaseNameDatabaseSQL.Strings6select dks,kks,sum(s) as s from spost group by dks,kks Left0Top  TTablesVertDatabaseNameDatabase	IndexNameKS	TableNameS.DBFLeft�Top  TTablesHorDatabaseNameDatabase	IndexNameKS	TableNameS.DBFLeftTop  TQueryRepQueryDatabaseNameDatabaseSQL.Strings4select * from customer a, orders b, items c, parts dwhere a.custno = b.custno  and b.orderno = c.orderno  and c.partno = d.partnoorder by a.company, orderno LeftPTop TFloatFieldRepQueryCustNo	FieldNameCustNo  TStringFieldRepQueryCompany	FieldNameCompanySize  TStringFieldRepQueryAddr1	FieldNameAddr1Size  TStringFieldRepQueryAddr2	FieldNameAddr2Size  TStringFieldRepQueryCity	FieldNameCitySize  TStringFieldRepQueryState	FieldNameState  TStringFieldRepQueryZip	FieldNameZipSize
  TStringFieldRepQueryCountry	FieldNameCountry  TStringFieldRepQueryPhone	FieldNamePhoneSize  TStringFieldRepQueryFAX	FieldNameFAXSize  TFloatFieldRepQueryTaxRate	FieldNameTaxRate  TStringFieldRepQueryContact	FieldNameContact  TDateTimeFieldRepQueryLastInvoiceDate	FieldNameLastInvoiceDate  TFloatFieldRepQueryOrderNo	FieldNameOrderNo  TFloatFieldRepQueryCustNo_1	FieldNameCustNo_1  TDateTimeFieldRepQuerySaleDate	FieldNameSaleDate  TDateTimeFieldRepQueryShipDate	FieldNameShipDate  TIntegerFieldRepQueryEmpNo	FieldNameEmpNo  TStringFieldRepQueryShipToContact	FieldNameShipToContact  TStringFieldRepQueryShipToAddr1	FieldNameShipToAddr1Size  TStringFieldRepQueryShipToAddr2	FieldNameShipToAddr2Size  TStringFieldRepQueryShipToCity	FieldName
ShipToCitySize  TStringFieldRepQueryShipToState	FieldNameShipToState  TStringFieldRepQueryShipToZip	FieldName	ShipToZipSize
  TStringFieldRepQueryShipToCountry	FieldNameShipToCountry  TStringFieldRepQueryShipToPhone	FieldNameShipToPhoneSize  TStringFieldRepQueryShipVIA	FieldNameShipVIASize  TStringField
RepQueryPO	FieldNamePOSize  TStringFieldRepQueryTerms	FieldNameTermsSize  TStringFieldRepQueryPaymentMethod	FieldNamePaymentMethodSize  TCurrencyFieldRepQueryItemsTotal	FieldName
ItemsTotal  TFloatFieldRepQueryTaxRate_1	FieldName	TaxRate_1  TCurrencyFieldRepQueryFreight	FieldNameFreight  TCurrencyFieldRepQueryAmountPaid	FieldName
AmountPaid  TFloatFieldRepQueryOrderNo_1	FieldName	OrderNo_1  TFloatFieldRepQueryItemNo	FieldNameItemNo  TFloatFieldRepQueryPartNo	FieldNamePartNo  TIntegerFieldRepQueryQty	FieldNameQty  TFloatFieldRepQueryDiscount	FieldNameDiscount  TFloatFieldRepQueryPartNo_1	FieldNamePartNo_1  TFloatFieldRepQueryVendorNo	FieldNameVendorNo  TStringFieldRepQueryDescription	FieldNameDescriptionSize  TFloatFieldRepQueryOnHand	FieldNameOnHand  TFloatFieldRepQueryOnOrder	FieldNameOnOrder  TCurrencyFieldRepQueryCost	FieldNameCost  TCurrencyFieldRepQueryListPrice	FieldName	ListPrice   	TDatabaseDatabaseDatabaseNameDatabase
DriverNameSTANDARDParams.StringsPath= SessionNameDefaultLeftpTop   