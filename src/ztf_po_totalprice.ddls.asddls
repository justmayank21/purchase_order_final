@EndUserText.label: 'table function for total'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ClientHandling.type: #CLIENT_DEPENDENT
define table function ztf_po_totalprice
returns
{
  key client             : abap.clnt;
  key purchase_order_num : zpurchase_order_num;
  key item_number        : zitem_number;
      //      vendor_num         : zvendor_number;
      @Semantics.amount.currencyCode : 'currency'
      unit_price         : zamount_field;
      currency           : zcurrency_field;
      quantity           : zquantity_field;
      unit               : zunit_field;
      Total_amount       : zamount_field;
      net_amount         : zamount_field;

}
implemented by method
  zcl_po_amdp=>get_data;