@AbapCatalog.sqlViewName: 'ZPL_GTOP_VAR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Gastos Presup Variables'
define view ZPL_GASTOS_PRE_VAR as select from /bic/azpl_a147 as a
        inner join zpl_ord_var as g
        on a.coorder =  g.coorder
{
key  a.comp_code,
key  a.co_area,
key  a.chrt_accts,
key  a.fiscyear,
key  a.fiscper,
key  a./bic/ztabla,
key  a.gl_account,
key  a.costcenter,
key  g.coorder,   
key  'gasto' as Tipo,   
     g./bic/zmar as TipoOrden,
     
      sum( a./bic/zquantity ) as Cantidad ,
     sum(a./bic/zpl_gastp ) as Gastos,
     cast('0' as abap.curr( 17, 3 ))  as Ventas
}
 
 group by 
 a.comp_code,
 a.co_area,
 a.chrt_accts,
 a.fiscyear, 
 a.fiscper, 
 a./bic/ztabla, 
a.costcenter,
a.gl_account,
g.coorder,
g./bic/zmar
