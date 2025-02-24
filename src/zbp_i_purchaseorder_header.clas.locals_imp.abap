CLASS lhc_header DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PUBLIC SECTION.
    CLASS-DATA:
      mt_header_create TYPE STANDARD TABLE OF zpo_header_table ,
      ms_header_create TYPE zpo_header_table,
      mt_header_delete TYPE STANDARD TABLE OF zpo_header_table ,
      ms_header_delete TYPE zpo_header_table,
      wa_header        TYPE zpo_header_table,
      mt_header_update TYPE STANDARD TABLE OF zpo_header_table ,
      ms_header_update TYPE zpo_header_table,
      mt_item_create   TYPE STANDARD TABLE OF zpo_item_table,
      ms_item_create   TYPE zpo_item_table,
      mt_quan_create   TYPE STANDARD TABLE OF zpo_quan_plant ,
      ms_quan_create   TYPE zpo_quan_plant,
      mt_item_delete   TYPE STANDARD TABLE OF zpo_item_table ,
      ms_item_delete   TYPE zpo_item_table,
      mt_quan_delete   TYPE STANDARD TABLE OF zpo_quan_plant ,
      ms_quan_delete   TYPE zpo_quan_plant,
      mt_action        TYPE STANDARD TABLE OF zpo_header_table ,
      mt_item_update   TYPE STANDARD TABLE OF zpo_item_table ,
      ms_item_update   TYPE zpo_item_table.
*      mt_material_create   TYPE STANDARD TABLE OF zmaterial_table WITH NON-UNIQUE DEFAULT KEY,
*      ms_material_create   TYPE zmaterial_table.

    METHODS validate_alphanumeric
      IMPORTING
        input_string    TYPE string
      RETURNING
        VALUE(is_valid) TYPE abap_boolean.


  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR header RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE header.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE header.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE header.

    METHODS read FOR READ
      IMPORTING keys FOR READ header RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK header.

    METHODS rba_Item FOR READ
      IMPORTING keys_rba FOR READ header\_Item FULL result_requested RESULT result LINK association_links.

    METHODS rba_Quan FOR READ
      IMPORTING keys_rba FOR READ header\_Quan FULL result_requested RESULT result LINK association_links.

    METHODS cba_Item FOR MODIFY
      IMPORTING entities_cba FOR CREATE header\_Item.

    METHODS cba_Quan FOR MODIFY
      IMPORTING entities_cba FOR CREATE header\_Quan.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR header RESULT result.

    METHODS approve FOR MODIFY
      IMPORTING keys FOR ACTION header~approve RESULT result.

    METHODS reject FOR MODIFY
      IMPORTING keys FOR ACTION header~reject RESULT result.

*    METHODS validate_order_creation_date
*      RETURNING VALUE(result) TYPE abap_boolean.

    METHODS validate_header_details
      IMPORTING
        vendor_name         TYPE zpo_header_table-vendor_name
        vendor_number       TYPE zpo_header_table-vendor_num
        order_creation_date TYPE datum
      EXPORTING
        error_message       TYPE string
      RETURNING
        VALUE(is_valid)     TYPE abap_boolean.


    METHODS validate_item_details
      IMPORTING
        item_number     TYPE zpo_item_table-item_number
        material_number TYPE zpo_item_table-material_number
        unit_price      TYPE zpo_item_table-unit_price
        currency        TYPE zpo_item_table-currency
      EXPORTING
        error_message   TYPE string
      RETURNING
        VALUE(is_valid) TYPE abap_boolean.

    METHODS validate_quan_details
      IMPORTING
        item_number     TYPE zpo_quan_plant-item_number
        material_number TYPE zpo_quan_plant-material_number
        quantity        TYPE zpo_quan_plant-quantity
        plant_number    TYPE zpo_quan_plant-plant_number
      EXPORTING
        error_message   TYPE string
      RETURNING
        VALUE(is_valid) TYPE abap_boolean.

    DATA lv_error_message TYPE string.





ENDCLASS.

CLASS lhc_header IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<header_data>).


      IF validate_header_details(
      EXPORTING
            vendor_name = <header_data>-VendorName
            vendor_number = <header_data>-VendorNum
            order_creation_date = <header_data>-OrderCreationDate
      IMPORTING
            error_message = lv_error_message
       ) = abap_false.

        APPEND VALUE #( %key = <header_data>-%key ) TO failed-header.
        APPEND VALUE #( %key = <header_data>-%key
                        %msg = new_message_with_text(
                        text = lv_error_message
                        severity = if_abap_behv_message=>severity-error
                       )
                        ) TO reported-header.


      ELSE.
        MOVE-CORRESPONDING <header_data> TO ms_header_create.
        DATA lv_max_purchase_order_number TYPE zpo_header_table-purchase_order_num.

        SELECT MAX( purchase_order_num )
        FROM zpo_header_table
        INTO @lv_max_purchase_order_number.

        IF lv_max_purchase_order_number IS INITIAL.
          lv_max_purchase_order_number =  9001.
        ELSE.
          lv_max_purchase_order_number = lv_max_purchase_order_number + 1.
        ENDIF.

        ms_header_create-purchase_order_num = lv_max_purchase_order_number.
        ms_header_create-vendor_num = <header_data>-VendorNum.
        ms_header_create-vendor_name = <header_data>-VendorName.
        ms_header_create-status = 'InProgress'.          "<header_data>-Status.
        ms_header_create-order_creation_date = <header_data>-OrderCreationDate.
        ms_header_create-description = <header_data>-Description.
        ms_header_create-created_by = <header_data>-CreatedBy.

        INSERT CORRESPONDING #( <header_data> ) INTO TABLE mapped-header.
        INSERT CORRESPONDING #( ms_header_create ) INTO TABLE mt_header_create.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.

  METHOD update.

    DATA(result)  = entities[ 1 ].

    SELECT * FROM zpo_header_table
    WHERE purchase_order_num = @result-PurchaseOrderNum
    INTO @DATA(wa_header).
    ENDSELECT.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

      IF wa_header IS NOT INITIAL.
        DATA(control) = <entity>-%control.

        IF control-VendorNum IS NOT INITIAL.
          wa_header-vendor_num = <entity>-VendorNum.
        ENDIF.

        IF control-VendorName IS NOT INITIAL.
          wa_header-vendor_name = <entity>-VendorName.
        ENDIF.

        IF control-Status IS NOT INITIAL.
          wa_header-status = <entity>-Status.
        ENDIF.

        IF control-OrderCreationDate IS NOT INITIAL.
          wa_header-order_creation_date = <entity>-OrderCreationDate.
        ENDIF.

        IF control-Description IS NOT INITIAL.
          wa_header-description = <entity>-Description.
        ENDIF.

        IF control-CreatedBy IS NOT INITIAL.
          wa_header-created_by = <entity>-CreatedBy.
        ENDIF.

      ENDIF.

      IF validate_header_details(
      EXPORTING
            vendor_name = wa_header-vendor_name
            vendor_number = wa_header-vendor_num
            order_creation_date = wa_header-order_creation_date
      IMPORTING
            error_message = lv_error_message
       ) = abap_false.

        APPEND VALUE #( %key = <entity>-%key ) TO failed-header.
        APPEND VALUE #( %key = <entity>-%key
                        %msg = new_message_with_text(
                        text = lv_error_message
                        severity = if_abap_behv_message=>severity-error
                       )
                        ) TO reported-header.
      ELSE.

        INSERT CORRESPONDING #( wa_header ) INTO TABLE mt_header_update.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD delete.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<header_delete>).
      ms_header_delete-purchase_order_num = <header_delete>-PurchaseOrderNum.
      INSERT CORRESPONDING #( ms_header_delete ) INTO TABLE mt_header_delete.

      SELECT * FROM zpo_item_table
      WHERE purchase_order_num = @<header_delete>-PurchaseOrderNum
      INTO TABLE @DATA(lt_items).
      LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<item>).
        ms_item_delete-purchase_order_num = <item>-purchase_order_num.
        ms_item_delete-item_number = <item>-item_number.
        INSERT CORRESPONDING #( ms_item_delete ) INTO TABLE mt_item_delete.
      ENDLOOP.

      SELECT * FROM zpo_quan_plant
      WHERE purchase_order_num = @<header_delete>-PurchaseOrderNum
      INTO TABLE @DATA(lt_quan).
      LOOP AT lt_quan ASSIGNING FIELD-SYMBOL(<quan>).
        ms_quan_delete-purchase_order_num = <quan>-purchase_order_num.
        ms_quan_delete-item_number = <quan>-item_number.
        INSERT CORRESPONDING #( ms_quan_delete ) INTO TABLE mt_quan_delete.
      ENDLOOP.



*      ms_item_delete-purchase_order_num = <header_delete>-PurchaseOrderNum.
*      INSERT CORRESPONDING #( ms_item_delete ) INTO TABLE mt_item_delete.
    ENDLOOP..

  ENDMETHOD.

  METHOD read.

    SELECT * FROM zi_purchaseOrder_header
            FOR ALL ENTRIES IN @keys
            WHERE PurchaseOrderNum = @keys-PurchaseOrderNum
            INTO CORRESPONDING FIELDS OF TABLE @result.

  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD rba_Item.
  ENDMETHOD.

  METHOD rba_Quan.
  ENDMETHOD.

  METHOD cba_Item.

    LOOP AT entities_cba ASSIGNING FIELD-SYMBOL(<item_detail>).
      LOOP AT <item_detail>-%target ASSIGNING FIELD-SYMBOL(<add_item>).


        IF validate_item_details(
        EXPORTING
               item_number     = <add_item>-ItemNumber
               material_number = <add_item>-MaterialNumber
               unit_price  = <add_item>-UnitPrice
               currency   =  <add_item>-Currency
        IMPORTING
               error_message = lv_error_message
             ) = abap_false.

          APPEND VALUE #( %key = <item_detail>-%key ) TO failed-item.
          APPEND VALUE #( %key = <item_detail>-%key
                          %msg = new_message_with_text(
                          text = lv_error_message "'Item Number and Material Number must not be blank'
                          severity = if_abap_behv_message=>severity-error
                         )
                          ) TO reported-item.

        ELSE.

          MOVE-CORRESPONDING <add_item> TO ms_item_create.

          ms_item_create-purchase_order_num = <item_detail>-PurchaseOrderNum.
          ms_item_create-item_number = <add_item>-ItemNumber.
          ms_item_create-material_number = <add_item>-MaterialNumber.
          ms_item_create-material_description = <add_item>-MaterialDescription.
          ms_item_create-currency = <add_item>-Currency.
          ms_item_create-unit_price = <add_item>-UnitPrice.

          APPEND ms_item_create TO mt_item_create.

*          SELECT SINGLE * FROM zmaterial_table
*          WHERE material_number = @<add_item>-MaterialNumber
*          INTO @DATA(ls_material).
*
*          IF sy-subrc <> 0.
*
*            ms_material_create-material_number = <add_item>-MaterialNumber.
*            ms_material_create-material_description = <add_item>-MaterialDescription.
*            INSERT zmaterial_table FROM @ms_material_create.
*          ENDIF.

        ENDIF.
      ENDLOOP.


    ENDLOOP.


  ENDMETHOD.

  METHOD cba_Quan.

    DATA lt_items TYPE TABLE OF zpo_item_table.

    SELECT *
     FROM zpo_item_table
     FOR ALL ENTRIES IN @entities_cba
     WHERE purchase_order_num = @entities_cba-PurchaseOrderNum
     INTO TABLE @lt_items.

    LOOP AT entities_cba ASSIGNING FIELD-SYMBOL(<quan_detail>).
      LOOP AT <quan_detail>-%target ASSIGNING FIELD-SYMBOL(<add_quan>).

        READ TABLE lt_items WITH KEY
            purchase_order_num = <quan_detail>-PurchaseOrderNum
            item_number        = <add_quan>-ItemNumber
            material_number    = <add_quan>-MaterialNumber
            TRANSPORTING NO FIELDS.

        IF sy-subrc <> 0.

          APPEND VALUE #( %key = <quan_detail>-%key ) TO failed-quantity.
          APPEND VALUE #( %key = <quan_detail>-%key
                          %msg = new_message_with_text(
                          text = 'Item Num and Mat Num must exist in the item table for this PO'
                          severity = if_abap_behv_message=>severity-error
                         )
                          ) TO reported-quantity.



        ELSE.
          IF validate_quan_details(
          EXPORTING
          item_number = <add_quan>-ItemNumber
          material_number = <add_quan>-MaterialNumber
          quantity  = <add_quan>-Quantity
          plant_number = <add_quan>-PlantNumber
          IMPORTING
          error_message = lv_error_message
           ) = abap_false.

            APPEND VALUE #( %key = <quan_detail>-%key ) TO failed-quantity.
            APPEND VALUE #( %key = <quan_detail>-%key
                            %msg = new_message_with_text(
                            text = lv_error_message
                            severity = if_abap_behv_message=>severity-error
                           )
                            ) TO reported-quantity.

          ELSE.
            MOVE-CORRESPONDING <add_quan> TO ms_quan_create.

            ms_quan_create-purchase_order_num = <quan_detail>-PurchaseOrderNum.
            ms_quan_create-item_number = <add_quan>-ItemNumber.
            ms_quan_create-material_number = <add_quan>-MaterialNumber.
            ms_quan_create-plant_number = <add_quan>-PlantNumber.
            ms_quan_create-plant_address = <add_quan>-PlantAddress.
            ms_quan_create-unit = <add_quan>-Unit.
            ms_quan_create-quantity = <add_quan>-Quantity.

            APPEND ms_quan_create TO mt_quan_create.
          ENDIF.
        ENDIF.
      ENDLOOP.


    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_features.

    READ ENTITIES OF zi_purchaseOrder_header IN LOCAL MODE
      ENTITY header
         FIELDS (  PurchaseOrderNum Status )
         WITH CORRESPONDING #( keys )
       RESULT DATA(lt_result)
       FAILED failed.

    result =
      VALUE #( FOR ls_stat IN lt_result
        ( %key = ls_stat-%key
          %features = VALUE #( %action-reject = COND #( WHEN ls_stat-Status = 'Rejected'
                                                        THEN if_abap_behv=>fc-o-disabled
                                                        ELSE if_abap_behv=>fc-o-enabled )

                        %action-approve = COND #( WHEN ls_stat-Status = 'Approved'
                                                        THEN if_abap_behv=>fc-o-disabled
                                                        ELSE if_abap_behv=>fc-o-enabled
                                                         )

                                                         )
         ) ).


  ENDMETHOD.

  METHOD approve.

    DATA(po_number) = keys[ 1 ]-PurchaseOrderNum.

    SELECT SINGLE  * FROM zpo_header_table WHERE purchase_order_num = @po_number INTO @ms_header_update.

    IF sy-subrc = 0.

      ms_header_update-status = 'Approved'.
      MODIFY zpo_header_table FROM @ms_header_update.

      IF sy-subrc = 0.
        DATA(final_result) = CORRESPONDING zi_purchaseorder_header( ms_header_create ).

        result = VALUE #( ( %key = keys[ 1 ]-%key
                            %param = final_result ) ).
      ENDIF.

    ENDIF.

*    LOOP AT lt_data INTO DATA(ls_data).
*      MOVE-CORRESPONDING ls_data TO ms_header_create.
*      ms_header_create-status = 'Approved'.
*      INSERT CORRESPONDING #( ms_header_create ) INTO TABLE mt_action.
*    ENDLOOP.

  ENDMETHOD.

  METHOD reject.

    DATA(po_number) = keys[ 1 ]-PurchaseOrderNum.

    SELECT SINGLE  * FROM zpo_header_table WHERE purchase_order_num = @po_number INTO @ms_header_update.

    IF sy-subrc = 0.

      ms_header_update-status = 'Rejected'.
      MODIFY zpo_header_table FROM @ms_header_update.

      IF sy-subrc = 0.
*      DATA(lv_result) = VALUE zi_purchaseorder_header(
*          PurchaseOrderNum    = ms_header_create-purchase_order_num,
*          VendorNum           = ms_header_create-vendor_num,
*          VendorName          = ms_header_create-vendor_name,
*          Status              = ms_header_create-status,
*          OrderCreationDate   = ms_header_create-order_creation_date,
*          Description         = ms_header_create-description,
*          CreatedBy           = ms_header_create-created_by
*        ).
        DATA(final_result) = CORRESPONDING zi_purchaseorder_header( ms_header_create ).

        result = VALUE #( ( %key = keys[ 1 ]-%key
                            %param = final_result ) ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD validate_header_details.

    DATA(current_date) = cl_abap_context_info=>get_system_date(  ).
    error_message = ''.

    IF vendor_number IS INITIAL.
      error_message = 'Vendor Number must not be blank'.
      is_valid = abap_false.
      RETURN.
    ELSE.
      DATA(lv_vendor_number_str) = CONV string( vendor_number ).
      IF validate_alphanumeric( input_string = lv_vendor_number_str ) = abap_false.
        error_message = 'Vendor Number should be alphanumeric'.
        is_valid = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    IF vendor_name IS INITIAL.
      error_message = 'Vendor Name must not be blank'.
      is_valid = abap_false.
      RETURN.
    ELSE.
      DATA(lv_vendor_name_str) =  vendor_name .
      CONDENSE lv_vendor_name_str NO-GAPS.
      DATA(regex) = '^[a-zA-Z. ]*$'.
      FIND PCRE regex IN lv_vendor_name_str.
      IF sy-subrc <> 0.
        error_message = 'Vendor Name should contain only alphabets'.
        is_valid = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    IF order_creation_date < current_date OR order_creation_date IS INITIAL.
      error_message = 'Order Creation Date must not be blank or past date'.
      is_valid = abap_false.
      RETURN.
    ENDIF.


    is_valid = abap_true.

  ENDMETHOD.

  METHOD validate_item_details.

    error_message = ''.

    IF item_number IS INITIAL.
      error_message = 'Item Number must not be blank'.
      is_valid = abap_false.
      RETURN.
*      else.
*      SELECT SINGLE @abap_true
*    FROM zpo_item_table
*    WHERE purchase_order_num = @ms_header_create-purchase_order_num
*      AND item_number = @item_number
*    INTO @DATA(lv_item_exists).
*
*    IF lv_item_exists = abap_true.
*      error_message = |Item Number { item_number } already exists for this Purchase Order|.
*      is_valid = abap_false.
*      RETURN.
*    ENDIF.
    ENDIF.

    IF material_number IS INITIAL.
      error_message = 'Material Number must not be blank'.
      is_valid = abap_false.
      RETURN.
    ELSE.
      DATA(lv_material_num) = CONV string( material_number ). " Convert to STRING
      IF validate_alphanumeric( input_string = lv_material_num ) = abap_false.
        error_message = 'Material Number should be alphanumeric'.
        is_valid = abap_false.
        RETURN.
      ENDIF.
    ENDIF.


    IF unit_price IS INITIAL.
      error_message = 'Unit Price must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    IF currency IS INITIAL.
      error_message = 'Currency must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.




    is_valid = abap_true.

  ENDMETHOD.




  METHOD validate_quan_details.

    error_message = ''.

    IF item_number IS INITIAL.
      error_message = 'Item Number must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    IF material_number IS INITIAL.
      error_message = 'Material Number must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    IF quantity IS INITIAL.
      error_message = 'Quantity must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    IF plant_number IS INITIAL.
      error_message = 'Plant Number must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    is_valid = abap_true.



  ENDMETHOD.

  METHOD validate_alphanumeric.

    DATA(regex) = '^[a-zA-Z0-9. ]*$'.
    FIND PCRE regex IN input_string.
    is_valid = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.

ENDCLASS.

CLASS lhc_item DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.
    CLASS-DATA:
      mt_item_delete     TYPE STANDARD TABLE OF zpo_item_table,
      ms_item_delete     TYPE zpo_item_table,
      mt_item_update     TYPE STANDARD TABLE OF zpo_item_table,
      ms_item_update     TYPE zpo_item_table,
      wa_item            TYPE zpo_item_table,
      ms_material_create TYPE zmaterial_table,
      ms_quan_delete     type zpo_quan_plant,
      mt_quan_delete     type STANDARD TABLE OF zpo_quan_plant.

    METHODS validate_alphanumeric
      IMPORTING
        input_string    TYPE string
      RETURNING
        VALUE(is_valid) TYPE abap_boolean.

  PRIVATE SECTION.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE item.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE item.

    METHODS read FOR READ
      IMPORTING keys FOR READ item RESULT result.

    METHODS rba_Header FOR READ
      IMPORTING keys_rba FOR READ item\_Header FULL result_requested RESULT result LINK association_links.

    METHODS validate_item_details
      IMPORTING
        item_number     TYPE zpo_item_table-item_number
        material_number TYPE zpo_item_table-material_number
        unit            TYPE zpo_item_table-unit_price
        currency        TYPE zpo_item_table-currency
      EXPORTING
        error_message   TYPE string
      RETURNING
        VALUE(is_valid) TYPE abap_boolean.

ENDCLASS.

CLASS lhc_item IMPLEMENTATION.

  METHOD update.

    DATA wa_item TYPE  zpo_item_table.
    DATA(result)  = entities[ 1 ].
    DATA lv_error_message TYPE string.

*    READ ENTITIES OF zi_purchaseorder_header IN LOCAL MODE
*    ENTITY item
*    FIELDS ( ItemNumber MaterialNumber MaterialDescription UnitPrice Currency )
*    WITH VALUE #( ( %key-PurchaseOrderNum = result-PurchaseOrderNum  %key-ItemNumber = result-ItemNumber ) )
*    RESULT DATA(lt_header)
*    FAILED DATA(failed_read)
*    REPORTED DATA(reported_read).
*
*    IF failed_read IS NOT INITIAL.
*      " Handle read failure
*      APPEND VALUE #( %key = result-%key ) TO failed-header.
*      APPEND VALUE #( %key = result-%key
*                      %msg = new_message_with_text(
*                        text = 'Failed to read item data'
*                        severity = if_abap_behv_message=>severity-error
*                      )
*                    ) TO reported-header.
*      RETURN.
*    ENDIF.


*DATA(wa_item) = lt_header[ 1 ].

    SELECT * FROM zpo_item_table
    WHERE purchase_order_num = @result-PurchaseOrderNum
    AND item_number = @result-ItemNumber
    INTO  @wa_item.
    ENDSELECT.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

      IF wa_item IS NOT INITIAL.
        DATA(control) = <entity>-%control.

        IF control-ItemNumber IS NOT INITIAL.
          wa_item-item_number = <entity>-ItemNumber.
        ENDIF.

        IF control-MaterialNumber IS NOT INITIAL.

*        IF wa_item-material_number <> <entity>-MaterialNumber.
*            " Delete the old material from zmaterial_table if no other items reference it
*            SELECT COUNT(*) FROM zpo_item_table
*            WHERE material_number = @wa_item-material_number
*            INTO @DATA(lv_material_count).
*
*            IF lv_material_count = 1.
*              DELETE FROM zmaterial_table WHERE material_number = @wa_item-material_number.
*            ENDIF.
*
*            " Insert the new material into zmaterial_table if it doesn't exist
*            SELECT SINGLE * FROM zmaterial_table
*            WHERE material_number = @<entity>-MaterialNumber
*            INTO @DATA(ls_material).
*
*            IF sy-subrc <> 0.
*              ms_material_create-material_number = <entity>-MaterialNumber.
*              ms_material_create-material_description = <entity>-MaterialDescription.
*              INSERT zmaterial_table FROM @ms_material_create.
*            ENDIF.
*            ENDIF.


          wa_item-material_number = <entity>-MaterialNumber.
        ENDIF.

        IF control-MaterialDescription IS NOT INITIAL.
          wa_item-material_description = <entity>-MaterialDescription.
*          UPDATE zmaterial_table
*          SET material_description = @<entity>-MaterialDescription
*          WHERE material_number = @wa_item-material_number.
        ENDIF.

        IF control-UnitPrice IS NOT INITIAL.
          wa_item-unit_price = <entity>-UnitPrice.
        ENDIF.

        IF control-Currency IS NOT INITIAL.
          wa_item-currency = <entity>-Currency.
        ENDIF.

      ENDIF.

      IF control-ItemNumber IS NOT INITIAL OR control-MaterialNumber IS NOT INITIAL.
        UPDATE zpo_quan_plant
        SET item_number = @wa_item-item_number,
            material_number = @wa_item-material_number
        WHERE purchase_order_num = @result-PurchaseOrderNum
          AND item_number = @result-ItemNumber.
      ENDIF.

      IF validate_item_details(
      EXPORTING
      item_number = wa_item-item_number
      material_number = wa_item-material_number
      unit = wa_item-unit_price
      currency = wa_item-Currency

      IMPORTING
      error_message = lv_error_message
      ) = abap_false.


        APPEND VALUE #( %key = <entity>-%key ) TO failed-item.
        APPEND VALUE #( %key = <entity>-%key
                        %msg = new_message_with_text(
                        text = lv_error_message
                        severity = if_abap_behv_message=>severity-error
                       )
                        ) TO reported-item.

      ELSE.
        INSERT CORRESPONDING #( wa_item ) INTO TABLE mt_item_update.
      ENDIF.
    ENDLOOP.



  ENDMETHOD.

  METHOD delete.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<item_delete>).
      ms_item_delete-purchase_order_num = <item_delete>-PurchaseOrderNum.
      ms_item_delete-item_number = <item_delete>-ItemNumber.

*      SELECT SINGLE material_number
*      FROM zpo_item_table
*      WHERE purchase_order_num = @<item_delete>-PurchaseOrderNum
*      AND item_number = @<item_delete>-ItemNumber
*      INTO @DATA(lv_material_number).
*
*      IF sy-subrc = 0.
*        " Check if the material is still referenced by other items
*        SELECT COUNT(*)
*        FROM zpo_item_table
*        WHERE material_number = @lv_material_number
*        INTO @DATA(lv_material_count).
*
*        IF lv_material_count = 1.
*          " No other items reference this material, so delete it from zmaterial_table
*          DELETE FROM zmaterial_table WHERE material_number = @lv_material_number.
*        ENDIF.
*      ENDIF.

*      SELECT * FROM zpo_quan_plant
*            WHERE purchase_order_num = @<item_delete>-PurchaseOrderNum
*            INTO TABLE @DATA(lt_quan).
*      LOOP AT lt_quan ASSIGNING FIELD-SYMBOL(<quan>).
*        ms_item_delete-purchase_order_num = <quan>-purchase_order_num.
*        ms_item_delete-item_number = <quan>-item_number.
*        INSERT CORRESPONDING #( ms_item_delete ) INTO TABLE mt_item_delete.
*      ENDLOOP.

      SELECT * FROM zpo_quan_plant
      WHERE purchase_order_num = @<item_delete>-PurchaseOrderNum
      and item_number = @<item_delete>-ItemNumber
      INTO TABLE @DATA(lt_quan).
      LOOP AT lt_quan ASSIGNING FIELD-SYMBOL(<quan>).
        ms_quan_delete-purchase_order_num = <quan>-purchase_order_num.
        ms_quan_delete-item_number = <quan>-item_number.
        INSERT CORRESPONDING #( ms_quan_delete ) INTO TABLE mt_quan_delete.
      ENDLOOP.

      INSERT CORRESPONDING #( ms_item_delete ) INTO TABLE mt_item_delete.
    ENDLOOP.

  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD rba_Header.
  ENDMETHOD.

  METHOD validate_item_details.

    error_message = ''.

    IF item_number IS INITIAL.
      error_message = 'Item Number must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    IF material_number IS INITIAL.
      error_message = 'Material Number must not be blank'.
      is_valid = abap_false.
      RETURN.
    ELSE.
      DATA(lv_material_num) = CONV string( material_number ). " Convert to STRING
      IF validate_alphanumeric( input_string = lv_material_num ) = abap_false.
        error_message = 'Material Number should be alphanumeric'.
        is_valid = abap_false.
        RETURN.
      ENDIF.
    ENDIF.


    IF unit IS INITIAL.
      error_message = 'Unit must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    IF currency IS INITIAL.
      error_message = 'Currency must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    is_valid = abap_true.

  ENDMETHOD.

  METHOD validate_alphanumeric.
    DATA(regex) = '^[a-zA-Z0-9. ]*$'.
    FIND PCRE regex IN input_string.
    is_valid = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS lhc_plant DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.
    CLASS-DATA:
      mt_quan_delete TYPE STANDARD TABLE OF zpo_quan_plant,
      ms_quan_delete TYPE zpo_quan_plant,
      mt_quan_update TYPE STANDARD TABLE OF zpo_quan_plant,
      ms_quan_update TYPE zpo_quan_plant,
      wa_quan        TYPE zpo_quan_plant.

  PRIVATE SECTION.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE quantity.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE quantity.

    METHODS read FOR READ
      IMPORTING keys FOR READ quantity RESULT result.

    METHODS rba_Header FOR READ
      IMPORTING keys_rba FOR READ quantity\_Header FULL result_requested RESULT result LINK association_links.

    METHODS validate_quan_details
      IMPORTING
        item_number     TYPE zpo_quan_plant-item_number
        material_number TYPE zpo_quan_plant-material_number
        quantity        TYPE zpo_quan_plant-quantity
        plant_number    TYPE zpo_quan_plant-plant_number
      EXPORTING
        error_message   TYPE string
      RETURNING
        VALUE(is_valid) TYPE abap_boolean.

    DATA lv_error_message TYPE string.

ENDCLASS.

CLASS lhc_plant IMPLEMENTATION.

  METHOD update.

    DATA(result)  = entities[ 1 ].

*    READ ENTITIES OF zi_purchaseOrder_header IN LOCAL MODE
*    ENTITY quantity
*    FIELDS ( PlantNumber PlantAddress Comments Quantity Unit  )
*    WITH VALUE #( ( %key-PurchaseOrderNum = result-PurchaseOrderNum %key-ItemNumber = result-ItemNumber ) )
*    RESULT DATA(lt_header)
*    FAILED DATA(failed_read)
*    REPORTED DATA(reported_read).
*
*    IF failed_read IS NOT INITIAL.
*      APPEND VALUE #( %key = result-%key ) TO failed-quantity.
*      APPEND VALUE #( %key = result-%key
*                      %msg = new_message_with_text(
*                        text = 'Failed to read quantity data'
*                        severity = if_abap_behv_message=>severity-error
*                      )
*                    ) TO reported-quantity.
*    ENDIF.

    SELECT * FROM zpo_quan_plant
    WHERE purchase_order_num = @result-PurchaseOrderNum
    AND item_number = @result-ItemNumber
    INTO @DATA(wa_quan).
    ENDSELECT.

*    SELECT SINGLE *
*        FROM zpo_item_table
*        WHERE purchase_order_num = @result-PurchaseOrderNum
*          AND item_number = @result-ItemNumber
*          AND material_number = @result-MaterialNumber
*        INTO @DATA(ls_item).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).


      IF wa_quan IS NOT INITIAL.
        DATA(control) = <entity>-%control.

        IF control-ItemNumber IS NOT INITIAL.
          wa_quan-item_number = <entity>-ItemNumber.
        ENDIF.

        IF control-MaterialNumber IS NOT INITIAL.
          wa_quan-material_number = <entity>-MaterialNumber.
        ENDIF.

        IF control-PlantNumber IS NOT INITIAL.
          wa_quan-plant_number = <entity>-PlantNumber.
        ENDIF.

        IF control-PlantAddress IS NOT INITIAL.
          wa_quan-plant_address = <entity>-PlantAddress.
        ENDIF.

        IF control-Quantity IS NOT INITIAL.
          wa_quan-quantity = <entity>-Quantity.
        ENDIF.

        IF control-Unit IS NOT INITIAL.
          wa_quan-unit = <entity>-Unit.
        ENDIF.

        IF control-Comments IS NOT INITIAL.
          wa_quan-comments = <entity>-Comments.
        ENDIF.
      ENDIF.


      IF validate_quan_details(
      EXPORTING
      item_number = wa_quan-item_number
      material_number = wa_quan-material_number
      plant_number = wa_quan-plant_number
      quantity = wa_quan-quantity
      IMPORTING
      error_message = lv_error_message
      ) = abap_false.

        APPEND VALUE #( %key = <entity>-%key ) TO failed-quantity.
        APPEND VALUE #( %key = <entity>-%key
                        %msg = new_message_with_text(
                          text = lv_error_message
                          severity = if_abap_behv_message=>severity-error
                        )
                      ) TO reported-quantity.

      ELSE.
        INSERT CORRESPONDING #( wa_quan ) INTO TABLE mt_quan_update.

      ENDIF.


    ENDLOOP.

  ENDMETHOD.

  METHOD delete.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<quan_delete>).
      ms_quan_delete-purchase_order_num = <quan_delete>-PurchaseOrderNum.
      ms_quan_delete-item_number = <quan_delete>-ItemNumber.
      INSERT CORRESPONDING #( ms_quan_delete ) INTO TABLE mt_quan_delete.
    ENDLOOP.

  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD rba_Header.
  ENDMETHOD.

  METHOD validate_quan_details.

    error_message = ''.

    IF item_number IS INITIAL.
      error_message = 'Item Number must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    IF material_number IS INITIAL.
      error_message = 'Material Number must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    IF plant_number IS INITIAL.
      error_message = 'Plant Number must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    IF quantity IS INITIAL.
      error_message = 'Quantity must not be blank'.
      is_valid = abap_false.
      RETURN.
    ENDIF.

    is_valid = abap_true.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZI_PURCHASEORDER_HEADER DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZI_PURCHASEORDER_HEADER IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.

    IF lhc_header=>mt_header_create IS NOT INITIAL.
      MODIFY zpo_header_table FROM TABLE @lhc_header=>mt_header_create.
    ENDIF.

    IF lhc_header=>mt_item_create IS NOT INITIAL.
      MODIFY zpo_item_table FROM TABLE @lhc_header=>mt_item_create.
    ENDIF.

    IF lhc_header=>mt_quan_create IS NOT INITIAL.
      MODIFY zpo_quan_plant FROM TABLE @lhc_header=>mt_quan_create.
    ENDIF.

    IF lhc_header=>mt_header_delete IS NOT INITIAL.
      DELETE zpo_header_table FROM TABLE @lhc_header=>mt_header_delete.
      DELETE zpo_item_table FROM TABLE @lhc_header=>mt_item_delete.
      DELETE zpo_quan_plant FROM TABLE @lhc_header=>mt_quan_delete.
*      DELETE zpo_item_table FROM TABLE @lhc_item=>mt_item_delete.
*      DELETE zpo_quan_plant FROM TABLE @lhc_plant=>mt_quan_delete.
    ENDIF.

    IF lhc_header=>mt_header_update IS NOT INITIAL.
      MODIFY zpo_header_table FROM TABLE @lhc_header=>mt_header_update.
    ENDIF.

    IF lhc_item=>mt_item_update IS NOT INITIAL.
      MODIFY zpo_item_table FROM TABLE @lhc_item=>mt_item_update.
    ENDIF.

    IF lhc_item=>mt_item_delete IS NOT INITIAL.
      DELETE zpo_item_table FROM TABLE @lhc_item=>mt_item_delete.
      DELETE zpo_quan_plant FROM TABLE @lhc_item=>mt_quan_delete.
    ENDIF.

    IF lhc_plant=>mt_quan_update IS NOT INITIAL.
      MODIFY zpo_quan_plant FROM TABLE @lhc_plant=>mt_quan_update.
    ENDIF.

    IF lhc_plant=>mt_quan_delete IS NOT INITIAL.
      DELETE zpo_quan_plant FROM TABLE @lhc_plant=>mt_quan_delete.
    ENDIF.

*    IF lhc_header=>ms_material_create IS NOT INITIAL.
*      MODIFY zmaterial_table FROM @lhc_header=>ms_material_create.
*    ENDIF.
*
*    IF lhc_item=>mt_item_update IS NOT INITIAL.
*      LOOP AT lhc_item=>mt_item_update ASSIGNING FIELD-SYMBOL(<item_update>).
*        UPDATE zmaterial_table
*        SET material_description = @<item_update>-material_description
*        WHERE material_number = @<item_update>-material_number.
*      ENDLOOP.
*    ENDIF.
*
*    IF lhc_item=>mt_item_delete IS NOT INITIAL.
*      LOOP AT lhc_item=>mt_item_delete ASSIGNING FIELD-SYMBOL(<item_delete>).
*        DELETE FROM zmaterial_table WHERE material_number = @<item_delete>-material_number.
*      ENDLOOP.
*    ENDIF.

  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
