@EndUserText.label: 'Custom Entity for Purchase Order'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_PO_CUSTOM_ENTITY'
@UI: {
  headerInfo: {
    typeName: 'Custom Report',
    typeNamePlural: 'Custom Reports',
    title: {
      type: #STANDARD,
      label: 'Purchase Order Details',
      value: 'purchase_order_num'
    },
    description: {
      type: #STANDARD,
      label: 'Vendor',
      value: 'vendor_name'
    }
  }
}
define root custom entity zc_custom_entity
{

      @EndUserText.label   : 'Purchase Order Number'
      @UI                  : {
        lineItem           : [
          { position       : 10, importance: #HIGH, label: 'Purchase Order Number' },
           { type           : #FOR_ACTION, dataAction: 'set_approve', label: 'Approve' },
      { type               : #FOR_ACTION, dataAction: 'set_reject', label: 'Reject' }
        ],
        identification     : [
          { position       : 10, label: 'Purchase Order Number' }
        ]
      }
      @UI.selectionField   : [{ position: 10 }]

      @Consumption.valueHelpDefinition: [{ entity: { name: 'zc_purchaseorder_header', element: 'PurchaseOrderNum' } }]
  key purchase_order_num   : zpurchase_order_num;

      @EndUserText.label   : 'Item Number'
      @UI                  : {
        lineItem           : [
          { position       : 11, importance: #HIGH, label: 'Item Number' }
        ],
        identification     : [
          { position       : 11, label: 'Item Number' }
        ]
      }

      @UI.selectionField   : [{ position: 11 }]
      @Consumption.valueHelpDefinition: [{ entity: { name: 'zc_purchaseorder_item', element: 'ItemNumber' } }]
  key item_number          : zitem_number;

      @EndUserText.label   : 'Vendor Number'
      @UI                  : {
        lineItem           : [
          { position       : 12, importance: #HIGH, label: 'Vendor Number' }
        ],
        identification     : [
          { position       : 12, label: 'Vendor Number' }
        ]
      }
      vendor_num           : zvendor_number;

      @EndUserText.label   : 'Vendor Name'
      @UI                  : {
        lineItem           : [
          { position       : 13, importance: #HIGH, label: 'Vendor Name' }
        ],
        identification     : [
          { position       : 13, label: 'Vendor Name' }
        ]
      }
      vendor_name          : zvendor_name;

      @EndUserText.label   : 'Status'
      @UI                  : {
        lineItem           : [
          { position       : 14, importance: #HIGH, label: 'Status' }
        ],
        identification     : [
          { position       : 14, label: 'Status' }
        ]
      }
      @UI.selectionField   : [{ position: 12 }]
      @Consumption.valueHelpDefinition: [{ entity: { name: 'zcds_status_15592', element: 'Value' },
        distinctValues     : true,
        additionalBinding  : [{ localElement: 'Status',
                              element: 'Description',
                              usage: #FILTER_AND_RESULT }] }]
      status               : zstatus_field;

      @EndUserText.label   : 'Order Creation Date'
      @UI                  : {
        lineItem           : [
          { position       : 15, importance: #HIGH, label: 'Order Creation Date' }
        ],
        identification     : [
          { position       : 15, label: 'Order Creation Date' }
        ]
      }
      order_creation_date  : zorder_creation_date;

      @EndUserText.label   : 'Description'
      @UI                  : {
        lineItem           : [
          { position       : 16, importance: #HIGH, label: 'Description' }
        ],
        identification     : [
          { position       : 16, label: 'Description' }
        ]
      }
      description          : zdescription_field;

      @EndUserText.label   : 'Created By'
      @UI                  : {
        lineItem           : [
          { position       : 17, importance: #HIGH, label: 'Created By' }
        ],
        identification     : [
          { position       : 17, label: 'Created By' }
        ]
      }
      created_by           : zcreated_by;

      @EndUserText.label   : 'Unit Price'
      @UI                  : {
        lineItem           : [
          { position       : 18, importance: #HIGH, label: 'Unit Price' }
        ],
        identification     : [
          { position       : 18, label: 'Unit Price' }
        ]
      }
      @Semantics.amount.currencyCode: 'currency'
      unit_price           : zamount_field;

      @EndUserText.label   : 'Currency'
      @UI                  : {
        lineItem           : [
          { position       : 19, importance: #HIGH, label: 'Currency' }
        ],
        identification     : [
          { position       : 19, label: 'Currency' }
        ]
      }
      currency             : zcurrency_field;

      @EndUserText.label   : 'Material Number'
      @UI                  : {
        lineItem           : [
          { position       : 20, importance: #HIGH, label: 'Material Number' }
        ],
        identification     : [
          { position       : 20, label: 'Material Number' }
        ]
      }
      material_number      : zmaterial_number;

      @EndUserText.label   : 'Material Description'
      @UI                  : {
        lineItem           : [
          { position       : 21, importance: #HIGH, label: 'Material Description' }
        ],
        identification     : [
          { position       : 21, label: 'Material Description' }
        ]
      }
      material_description : zmaterial_description;

      @EndUserText.label   : 'Quantity'
      @UI                  : {
        lineItem           : [
          { position       : 22, importance: #HIGH, label: 'Quantity' }
        ],
        identification     : [
          { position       : 22, label: 'Quantity' }
        ]
      }
      @Semantics.quantity.unitOfMeasure: 'unit'
      quantity             : zquantity_field;

      @EndUserText.label   : 'Unit'
      @UI                  : {
        lineItem           : [
          { position       : 23, importance: #HIGH, label: 'Unit' }
        ],
        identification     : [
          { position       : 23, label: 'Unit' }
        ]
      }
      unit                 : zunit_field;

      @EndUserText.label   : 'Plant Number'
      @UI                  : {
        lineItem           : [
          { position       : 24, importance: #HIGH, label: 'Plant Number' }
        ],
        identification     : [
          { position       : 24, label: 'Plant Number' }
        ]
      }

      plant_number         : zplant_number;

      @EndUserText.label   : 'Plant Address'
      @UI                  : {
        lineItem           : [
          { position       : 25, importance: #HIGH, label: 'Plant Address' }
        ],
        identification     : [
          { position       : 25, label: 'Plant Address' }
        ]
      }
      plant_address        : zplant_address;

}
