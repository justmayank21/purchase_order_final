@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'header cds for purchase order header'
@Metadata.ignorePropagatedAnnotations: true
define root view entity zi_purchaseOrder_header
  as select from zpo_header_table
  composition [0..*] of zi_purchaseOrder_item as _item
  composition [1..*] of zi_quantity_plant     as _quan
  association [0..*] to zi_purchaseOrder_item as _items on $projection.PurchaseOrderNum = _items.PurchaseOrderNum
                                                     
association [1..*] to ztf_cds_po as _totalprice
  on $projection.PurchaseOrderNum = _totalprice.purchase_order_num
{
  key purchase_order_num  as PurchaseOrderNum,
      vendor_num          as VendorNum,
      vendor_name         as VendorName,
      status              as Status,
      case status
      when 'Approved'  then 3    -- 'open'       | 2: yellow colour
      when 'Rejected'  then 1    -- 'accepted'   | 3: green colour
      when 'InProgress'  then 2    -- 'rejected'   | 1: red colour
      else 0    -- 'nothing'    | 0: unknown
      end                 as OverallStatus,
      order_creation_date as OrderCreationDate,
      description         as Description,
      created_by          as CreatedBy,
      locallastchange     as Locallastchange,
      _items,
      
      _totalprice,
      _item,
      _quan
}
