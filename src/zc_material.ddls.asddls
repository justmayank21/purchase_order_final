@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'projection for material'
@Metadata.ignorePropagatedAnnotations: true
define root view entity zc_material as projection on zi_material
{
    key MaterialNumber,
    MaterialDescription
}
