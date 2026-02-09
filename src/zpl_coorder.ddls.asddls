@AbapCatalog.sqlViewName: 'ZPL_COORDER_VIEW'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Ordenes Internas'
define view ZPL_COORDER as select distinct
 from /bi0/mcoorder as a
{
   key right( a.coorder, 10 ) as ref_doc,
   key a.coorder,
   key a.resp_user,
   key a./bic/zmar

}where 
( a.coord_type != 'RM1' and
  a.coord_type != 'ZS01' and
  a.coord_type != ' ' ) and
  a./bic/zmar != ' ' and
  a./bic/zdir = '' and
  a.objvers = 'A'
  
