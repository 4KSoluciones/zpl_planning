@AbapCatalog.sqlViewName: 'ZPL_ORD_VARQ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Ordenes Internas Variables Cantidad'
define view ZPL_COORDER_VARQ as select from /bi0/mcoorder as a
{
    key  a.comp_code,
    key right( a.coorder, 10 ) as ref_doc,
    key a.coorder,
    a./bic/zmar,
    a.coord_type

}
where ( a./bic/zmar = 'VAC')  
