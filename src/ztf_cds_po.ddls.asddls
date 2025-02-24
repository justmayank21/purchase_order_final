@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'cds for table function'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ztf_cds_po as select from ztf_po_totalprice
{
    key purchase_order_num,
    key item_number,
//    vendor_num,
    @Semantics.amount.currencyCode : 'currency'
    unit_price,
    currency,
    @Semantics.quantity.unitOfMeasure : 'unit'
    quantity,
    unit,
    @Semantics.amount.currencyCode : 'currency'
    Total_amount,
    @Semantics.amount.currencyCode : 'currency'
    net_amount
}
