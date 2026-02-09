@AbapCatalog.sqlViewName: 'ZPL_GASTO_VARQ_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Gastos Variables Cantidad'
define view ZPL_GASTOS_VARQ  
as select from ZPL_GASTOS_VARQ_SUM as a
{
key  a.comp_code,
key  a.co_area,
key  a.chrt_accts,
key  a.fiscyear,
key  a.fiscper,
key  a./bic/ztabla,
key  a.curtype,
key  a.gl_account,
key  a.profit_ctr,  
key  a.coorder,
key  a.Tipo, 
key  a.TipoOrden,  
     sum( a.Cantidad ) as Cantidad ,
     sum( a.Gastos ) as Gastos,
     sum( a.Ventas )  as Ventas
} 
 group by 
 a.comp_code,
 a.co_area,
 a.chrt_accts,
 a.fiscyear, 
 a.fiscper, 
 a./bic/ztabla,
 a.curtype,
a.profit_ctr,
a.gl_account,
a.coorder,
a.Tipo,
a.TipoOrden
