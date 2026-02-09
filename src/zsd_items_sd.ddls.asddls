@AbapCatalog.sqlViewName: 'ZSD_ITEMS_FC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Items por factura SD'
define view ZSD_ITEMS_FACT as select from /bic/azsd_o0300 as a{

  key a.bill_num , key a.material,
  count(*) as Cantidad 
    
} group by a.bill_num, a.material
