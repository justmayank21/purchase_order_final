@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'item cds for purchase order'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zi_purchaseOrder_item as select from zpo_item_table
association to parent zi_purchaseOrder_header as _header on $projection.PurchaseOrderNum = _header.PurchaseOrderNum
association [0..1] to ztf_cds_po as _totalprice
  on $projection.PurchaseOrderNum = _totalprice.purchase_order_num
  and $projection.ItemNumber = _totalprice.item_number
{
    key purchase_order_num as PurchaseOrderNum,
    key item_number as ItemNumber,
    material_number as MaterialNumber,
    material_description as MaterialDescription,
    @Semantics.amount.currencyCode: 'Currency'
    unit_price as UnitPrice,
    currency as Currency,
    _totalprice,
    _header
}
