@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'item projection for purchase order'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@Search.searchable: true
define view entity zc_purchaseorder_item as projection on zi_purchaseOrder_item
{
    key PurchaseOrderNum,
    key ItemNumber,
    @Search.defaultSearchElement: true
    @Search.fuzzinessThreshold: 0.5
    MaterialNumber,
    MaterialDescription,
    @Semantics.amount.currencyCode: 'Currency'
    UnitPrice,
    Currency,
    @Semantics.amount.currencyCode : 'Currency'
     _totalprice.Total_amount as TotalAmount,
    /* Associations */
    _header : redirected to parent zc_purchaseorder_header
}
