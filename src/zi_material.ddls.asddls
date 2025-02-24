@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'interface for material'
@Metadata.ignorePropagatedAnnotations: true
define root view entity zi_material as select from zmaterial_table

{
    key material_number as MaterialNumber,
    material_description as MaterialDescription
   
}
