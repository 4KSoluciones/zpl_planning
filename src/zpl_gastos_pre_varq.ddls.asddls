@AbapCatalog.sqlViewName: 'ZPL_GTOP_VARQ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Gastos Presup Variables Cantidad'
define view ZPL_GASTOS_PRE_VARQ as select from /bic/azpl_a177 as a
        inner join zpl_ord_varq as g
        on a.coorder =  g.coorder
{
key  a.comp_code,
key  a.co_area,
key  a.chrt_accts,
key  a.fiscyear,
key  a.fiscper,
key  a./bic/ztabla,
//key  a.curtype,
key  a.gl_account,
key  a.costcenter,
key  g.coorder,   
key  'gasto' as Tipo,   
     g./bic/zmar as TipoOrden,
      sum( a./bic/zpl_qinpu) as Cantidad ,
     sum(a./bic/zpl_gastp ) as Gastos,
     cast('0' as abap.curr( 17, 3 ))  as Ventas
}   where 
    a.gl_account  between '0060000000' and '0069999999' 

//and a.curtype = '10'

and a.chrt_accts = 'SQS'

 group by 
 a.comp_code,
 a.co_area,
 a.chrt_accts,
 a.fiscyear, 
 a.fiscper, 
 a./bic/ztabla,
// a.curtype,
a.costcenter,
a.gl_account,
g.coorder,
g./bic/zmar
