@AbapCatalog.sqlViewName: 'ZPL_GASTO_VARQ_S'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Gastos Ordenes Variables Cantidad Sum'
@Analytics.dataExtraction.enabled: true            // Enables ODP Extraction
@Analytics.dataCategory: #FACT                     // Declares as a Fact View for Analytics
define view ZPL_GASTOS_VARQ_SUM as select from /bic/azpca_o01n7 as a
        inner join zpl_ord_varq as g
        on a.refer_doc =  g.ref_doc
        left outer join zpl_order_conv as b
        on  b.comp_code = a.comp_code
        and b.coord_type = g.coord_type
        and b.gl_account = a.gl_account
        and b.zmar       = g./bic/zmar
        left outer join /bi0/mgl_account as c on a.gl_account = c.gl_account and c.objvers = 'A' and c.chrt_accts = 'SQS'
{
key  a.comp_code,
key  a.co_area,
key  a.chrt_accts,
key  a.fiscyear,
key  cast( Concat( Concat( substring(a.calmonth, 1, 4), '0' ),substring(a.calmonth, 5, 2)) as abap.numc(7) ) as fiscper,
key  a./bic/ztabla,
key  a.curtype,
key  a.gl_account,
key  a.profit_ctr,
key  g.coorder as OrdenOrig,
key  b.coorder as OrdenAgrup,
key   case when  not b.coorder  = '' then b.coorder
        else g.coorder end as coorder,   

key  'gasto' as Tipo,   

g./bic/zmar as TipoOrden,
 g.coord_type as ClaseOrden,

     sum( a.quantity ) as Cantidad ,
     sum(a./bic/zco_val ) as Gastos,
     cast('0' as abap.curr( 17, 3 ))  as Ventas
}   where 
    a.gl_account  between '0060000000' and '0069999999' 

and a.curtype = '10'
and a./bic/ztabla = 'R3'
and a.chrt_accts = 'SQS'
and c./bic/znivel1 = '07'

 group by 
 a.comp_code,
 a.co_area,
 a.chrt_accts,
 a.fiscyear, 
 a.calmonth, 
 a./bic/ztabla,
 a.curtype,
a.profit_ctr,
a.gl_account,
b.coorder,
g.coorder,
g.coord_type,
g./bic/zmar
