@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'projection for quantity and plant'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity zc_quantity_plant as projection on zi_quantity_plant
{
    key PurchaseOrderNum,
    key ItemNumber,
    MaterialNumber,
     @Semantics.quantity.unitOfMeasure : 'Unit'
    Quantity,
    Unit,
    PlantNumber,
    PlantAddress,
    Comments,
    _item.Currency,
    @Semantics.amount.currencyCode : 'currency'
    _totalprice.Total_amount as TotalAmount,
    _header : redirected to parent zc_purchaseorder_header
     
}
