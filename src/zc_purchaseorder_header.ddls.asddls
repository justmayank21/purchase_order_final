@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'header projection for purchase order'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@Search.searchable: true
define root view entity zc_purchaseorder_header as projection on zi_purchaseOrder_header
{
@Search.defaultSearchElement: true
@Search.fuzzinessThreshold: 0.5
    key PurchaseOrderNum,
    @Search.defaultSearchElement: true
@Search.fuzzinessThreshold: 0.5
    VendorNum,
    VendorName,
    Status,
    OverallStatus,
    OrderCreationDate,
    Description,
    CreatedBy,
    Locallastchange,   
//    _items.Currency as Currency,
//    @Semantics.amount.currencyCode: 'Currency'   
    _totalprice,
    _item : redirected to composition child zc_purchaseorder_item,
    _quan : redirected to composition child zc_quantity_plant
}
