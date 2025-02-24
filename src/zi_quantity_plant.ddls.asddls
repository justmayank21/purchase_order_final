@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'cds for quantity and plant'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zi_quantity_plant as select from zpo_quan_plant
association to parent zi_purchaseOrder_header as _header on $projection.PurchaseOrderNum = _header.PurchaseOrderNum
association [0..1] to zi_purchaseOrder_item as _item on $projection.PurchaseOrderNum = _item.PurchaseOrderNum
                                                      and $projection.ItemNumber = _item.ItemNumber
association [0..1] to ztf_cds_po as _totalprice
  on $projection.PurchaseOrderNum = _totalprice.purchase_order_num
  and $projection.ItemNumber = _totalprice.item_number
{
    key purchase_order_num as PurchaseOrderNum,
    key item_number as ItemNumber,
    material_number as MaterialNumber,
    @Semantics.quantity.unitOfMeasure : 'Unit'
    quantity as Quantity,
    unit as Unit,
    plant_number as PlantNumber,
    plant_address as PlantAddress,
    comments as Comments,
    _item,
    _totalprice,
    _header
}
