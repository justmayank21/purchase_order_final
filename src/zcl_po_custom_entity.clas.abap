CLASS zcl_po_custom_entity DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_po_custom_entity IMPLEMENTATION.


  METHOD if_rap_query_provider~select.

  DATA: lt_response TYPE TABLE OF zc_custom_entity,
          ls_response TYPE zc_custom_entity.

    DATA: "ls_docno TYPE if_rap_query_filter=>ty_range_option,
          lr_docno TYPE if_rap_query_filter=>tt_range_option,
          "ls_supno TYPE if_rap_query_filter=>ty_range_option,
          lr_supno TYPE if_rap_query_filter=>tt_range_option,
          "ls_status TYPE if_rap_query_filter=>ty_range_option,
          lr_status TYPE if_rap_query_filter=>tt_range_option.

    TRY.

        DATA(lv_data_req) = io_request->is_data_requested(  ).
        DATA(lv_top) = io_request->get_paging(  )->get_page_size(  ).
        DATA(lv_skip) = io_request->get_paging(  )->get_offset(  ).
        DATA(lt_fields) = io_request->get_requested_elements(  ).
        DATA(lt_sort) = io_request->get_sort_elements(  ).
        DATA(lv_page_size) = io_request->get_paging(  )->get_page_size( ).
        DATA(lv_max_row) = COND #( WHEN lv_page_size = if_rap_query_paging=>page_size_unlimited THEN 0 ELSE lv_page_size ).
        DATA(lv_condition) = io_request->get_filter(  )->get_as_ranges( iv_drop_null_comparisons = abap_true ).
        DATA(lv_filter_cond) = io_request->get_parameters(  ).

*        lv_max_row = lv_skip + lv_top.
*        IF lv_skip > 0.
*          lv_skip = lv_skip + 1.
*        ENDIF.

        SORT lv_condition BY name.

*        READ TABLE lv_condition WITH KEY name = 'PURCHASE_ORDER_NUM' INTO DATA(ls_key) BINARY SEARCH.
*        IF sy-subrc IS INITIAL AND lines( ls_key-range ) = 1.
*          DATA(lt_range_value) = ls_key-range.
*          LOOP AT lt_range_value INTO DATA(ls_range_value).
*
*            ls_docno-sign = ls_range_value-sign.
*            ls_docno-option = ls_range_value-option.
*            ls_docno-low = ls_range_value-low.
*            ls_docno-high = ls_range_value-high.
*
*            APPEND ls_docno TO lr_docno.
*          ENDLOOP.
*        ENDIF.

        LOOP AT lv_condition INTO DATA(ls_condition).
          CASE ls_condition-name.
            WHEN 'PURCHASE_ORDER_NUM'.
              LOOP AT ls_condition-range INTO DATA(ls_range_value).
                DATA(ls_docno) = VALUE if_rap_query_filter=>ty_range_option(
                  sign   = ls_range_value-sign
                  option = ls_range_value-option
                  low    = ls_range_value-low
                  high   = ls_range_value-high
                ).
                APPEND ls_docno TO lr_docno.
              ENDLOOP.

            WHEN 'ITEM_NUMBER'.
              LOOP AT ls_condition-range INTO DATA(ls_range).
                DATA(ls_supno) = VALUE if_rap_query_filter=>ty_range_option(
                  sign   = ls_range-sign
                  option = ls_range-option
                  low    = ls_range-low
                  high   = ls_range-high
                ).
                APPEND ls_supno TO lr_supno.
              ENDLOOP.

            WHEN 'STATUS'.
              LOOP AT ls_condition-range INTO DATA(ls_range_status).
                DATA(ls_status) = VALUE if_rap_query_filter=>ty_range_option(
                  sign   = ls_range_status-sign
                  option = ls_range_status-option
                  low    = ls_range_status-low
                  high   = ls_range_status-high
                ).
                APPEND ls_status TO lr_status.
              ENDLOOP.
          ENDCASE.
        ENDLOOP.

        SELECT a~purchase_order_num,
               a~vendor_num,
               a~vendor_name,
               a~status,
               a~order_creation_date,
               a~description,
               a~created_by,
               b~item_number,
               b~material_number,
               b~material_description,
               b~unit_price,
               b~currency,
               c~quantity,
               c~unit,
               c~plant_number,
               c~plant_address
          FROM zpo_header_table AS a
          LEFT OUTER JOIN zpo_item_table AS b
            ON b~purchase_order_num = a~purchase_order_num
          LEFT OUTER JOIN zpo_quan_plant AS c
            ON c~purchase_order_num = a~purchase_order_num
            and c~item_number = b~item_number
          WHERE a~purchase_order_num IN @lr_docno
            AND b~item_number IN @lr_supno
            and a~status in @lr_status
          ORDER BY a~purchase_order_num ASCENDING
          INTO TABLE @DATA(lt_header).

        IF lv_data_req IS INITIAL.
          IF io_request->is_total_numb_of_rec_requested( ).
            io_response->set_total_number_of_records( lines( lt_header ) ).
            RETURN.
          ENDIF.
        ENDIF.

        LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<fs_output>)
          FROM lv_skip TO lv_max_row.
          MOVE-CORRESPONDING <fs_output> TO ls_response.
          APPEND ls_response TO lt_response.
          CLEAR ls_response.
        ENDLOOP.

        TRY.
            io_response->set_total_number_of_records( lines( lt_response ) ).
            io_response->set_data( lt_response ).
          CATCH cx_rap_query_provider INTO DATA(lx_new_root).
            DATA(lv_text2) = lx_new_root->get_text( ).
        ENDTRY.

      CATCH cx_root INTO DATA(cx_dest).
        DATA(lv_text) = cx_dest->get_text( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
