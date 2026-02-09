@AbapCatalog.sqlViewName: 'ZPL_GASTOREALSUM'
@AbapCatalog.compiler.compareFilter: true
@VDM:{viewType: #CONSUMPTION}
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Gastos Reales Agregados'
define view ZPL_GASTOS_REAL_SUM as select from /bic/azpca_o01n7 as a
        inner join ZPL_COORDER as g
        on a.refer_doc =  g.ref_doc
{
key  a.comp_code,
key  a.co_area,
//key  a.chrt_accts,
//key  a.fiscyear,
//key  a.fiscper,
//key  a./bic/ztabla,
//key  a.curtype,
key  a.gl_account,
key  a.costcenter,
key  g.coorder,   
//key  'gasto' as Tipo,
//     g./bic/zmar as TipoOrden,
//     sum( a.quantity ) as Cantidad ,
     sum(a./bic/zco_val ) as Gastos
//     cast('0' as abap.curr( 17, 3 ))  as Ventas
}   where 
    a.gl_account  between '0060000000' and '0069999999' 
     and ( ( a.fiscper between '2022010' and '2022012' )
 or ( a.fiscper between '2023001' and '2023009' ) )

and a.curtype = '10'
and a./bic/ztabla = 'R3'
and a.chrt_accts = 'SQS'

 group by 
 a.comp_code,
 a.co_area,
// a.chrt_accts,
// a.fiscyear, //
// a.fiscper, 
// a./bic/ztabla,
// a.curtype,
a.costcenter,
a.gl_account,
g.coorder
//g./bic/zmar
