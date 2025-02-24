CLASS zcl_total_price DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_sadl_exit_calc_element_read.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_total_price IMPLEMENTATION.
  METHOD if_sadl_exit_calc_element_read~calculate.

   DATA: lt_calculated_data TYPE STANDARD TABLE OF zc_quantity_plant,
          lt_item_data       TYPE STANDARD TABLE OF zpo_item_table,
          lv_total_price     TYPE int4.

    " Retrieve the input data
    lt_calculated_data = CORRESPONDING #( it_original_data ).

    " Retrieve the UnitPrice from zpo_item_table
    SELECT purchase_order_num, item_number
      FROM zpo_item_table
      FOR ALL ENTRIES IN @lt_calculated_data
      WHERE purchase_order_num = @lt_calculated_data-PurchaseOrderNum
        AND item_number = @lt_calculated_data-ItemNumber
         INTO TABLE @lt_item_data.

    " Calculate the total_price
    LOOP AT lt_calculated_data ASSIGNING FIELD-SYMBOL(<fs_calculated_data>).
      READ TABLE lt_item_data ASSIGNING FIELD-SYMBOL(<fs_item_data>)
        WITH KEY purchase_order_num = <fs_calculated_data>-PurchaseOrderNum
                 item_number = <fs_calculated_data>-ItemNumber.
      IF sy-subrc = 0.
        lv_total_price = <fs_calculated_data>-Quantity * <fs_item_data>-unit_price.
        <fs_calculated_data>-total_price = lv_total_price.
      ELSE.
        <fs_calculated_data>-total_price = 0. " or handle error as needed
      ENDIF.
    ENDLOOP.

    " Return the calculated data
    ct_calculated_data = lt_calculated_data.

  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

  ENDMETHOD.

ENDCLASS.
