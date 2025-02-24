CLASS zcl_po_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb .
    CLASS-METHODS: get_data FOR TABLE FUNCTION ztf_po_totalprice.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_po_amdp IMPLEMENTATION.
  METHOD get_data BY DATABASE FUNCTION
   FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING zpo_item_table zpo_quan_plant.

    RETURN
    select
      po.client as client,
      po.purchase_order_num as purchase_order_num,
      po.item_number as item_number,
      po.unit_price as unit_price,
      po.currency as currency,
      qty.quantity as quantity,
      qty.unit as unit,
      po.unit_price * qty.quantity as Total_amount,
      sum( ( po.unit_price * qty.quantity ) + ( po.unit_price * qty.quantity ) ) as net_amount
    from
      zpo_item_table as po
    inner join
      zpo_quan_plant as qty
    on
*    po.client = qty.client
     po.purchase_order_num = qty.purchase_order_num
     and po.item_number = qty.item_number
     group by
     po.client,
     po.purchase_order_num, po.item_number, po.unit_price,po.currency, qty.quantity,qty.unit;

*       where po.client = SESSION_CONTEXT( 'client' );

  endmethod.

ENDCLASS.
