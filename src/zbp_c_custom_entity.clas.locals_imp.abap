CLASS lhc_zc_custom_entity DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PUBLIC SECTION.
    CLASS-DATA:
    ms_header_update TYPE zpo_header_table,
      ms_header_create TYPE zpo_header_table.

  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zc_custom_entity RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE zc_custom_entity.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE zc_custom_entity.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE zc_custom_entity.

    METHODS read FOR READ
      IMPORTING keys FOR READ zc_custom_entity RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zc_custom_entity.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zc_custom_entity RESULT result.

    METHODS set_approve FOR MODIFY
      IMPORTING keys FOR ACTION zc_custom_entity~set_approve RESULT result.

    METHODS set_reject FOR MODIFY
      IMPORTING keys FOR ACTION zc_custom_entity~set_reject RESULT result.

ENDCLASS.

CLASS lhc_zc_custom_entity IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.

*  READ ENTITIES OF zc_custom_entity IN LOCAL MODE
*      ENTITY zc_custom_entity
*         FIELDS (  purchase_order_num status )
*         WITH CORRESPONDING #( keys )
*       RESULT DATA(ls_result)
*       FAILED failed.

  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD get_instance_features.

  READ ENTITIES OF zc_custom_entity IN LOCAL MODE
      ENTITY zc_custom_entity
         FIELDS (  purchase_order_num status )
         WITH CORRESPONDING #( keys )
       RESULT DATA(lt_result)
       FAILED failed.

*       SELECT * FROM zc_custom_entity
*            FOR ALL ENTRIES IN @keys
*            WHERE PurchaseOrderNum = @keys-PurchaseOrderNum
*            INTO CORRESPONDING FIELDS OF TABLE @result.



    result =
      VALUE #( FOR ls_stat IN lt_result
        ( %key = ls_stat-%key
          %features = VALUE #( %action-set_reject = COND #( WHEN ls_stat-Status = 'Rejected'
                                                        THEN if_abap_behv=>fc-o-disabled
                                                        ELSE if_abap_behv=>fc-o-enabled )

                        %action-set_approve = COND #( WHEN ls_stat-Status = 'Approved'
                                                        THEN if_abap_behv=>fc-o-disabled
                                                        ELSE if_abap_behv=>fc-o-enabled
                                                         )

                                                         )
         ) ).

  ENDMETHOD.

  METHOD set_approve.

  DATA(po_number) = keys[ 1 ]-purchase_order_num.

    SELECT SINGLE  * FROM zpo_header_table WHERE purchase_order_num = @po_number INTO @ms_header_update.

    IF sy-subrc = 0.

      ms_header_update-status = 'Approved'.
      MODIFY zpo_header_table FROM @ms_header_update.

      IF sy-subrc = 0.
        DATA(final_result) = CORRESPONDING zc_custom_entity( ms_header_create ).

        result = VALUE #( ( %key = keys[ 1 ]-%key
                            %param = final_result ) ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD set_reject.

  DATA(po_number) = keys[ 1 ]-purchase_order_num.

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
        DATA(final_result) = CORRESPONDING zc_custom_entity( ms_header_create ).

        result = VALUE #( ( %key = keys[ 1 ]-%key
                            %param = final_result ) ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZC_CUSTOM_ENTITY DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZC_CUSTOM_ENTITY IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
