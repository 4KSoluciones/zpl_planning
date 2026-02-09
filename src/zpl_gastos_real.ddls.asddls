@AbapCatalog.sqlViewName: 'ZPL_GASTOS_VIEW'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Gastos Reales para PxQ'
@Analytics.dataExtraction.enabled: true            // Enables ODP Extraction
@Analytics.dataCategory: #FACT                     // Declares as a Fact View for Analytics
define view ZPL_GASTOS_REAL 
as select from /bic/azpca_o01n7 as a
        inner join ZPL_COORDER_PXQ as g
        on a.refer_doc =  g.ref_doc 
        left outer join /bi0/mgl_account as c on a.gl_account = c.gl_account and c.objvers = 'A' and c.chrt_accts = 'SQS'
{
key  a.comp_code,
//key  a.doc_num,
key  a.co_area,
key  a.chrt_accts,
key  a.fiscyear, 
key  cast( Concat( Concat( substring(a.calmonth, 1, 4), '0' ),substring(a.calmonth, 5, 2)) as abap.numc(7) ) as fiscper,
key  a./bic/ztabla,
key  a.curtype,
key  a.gl_account,
key  cast( a.profit_ctr as /bi0/oicostcenter ) as costcenter,
key  g.coorder,   
key  'gasto' as Tipo,   
     g./bic/zmar as TipoOrden,
     sum( a.quantity ) as Cantidad ,
     sum(a./bic/zco_val ) as Gastos,
     cast('0' as abap.curr( 17, 3 ))  as Ventas
}   where 
    a.gl_account  between '0060000000' and '0069999999' 
    and c./bic/znivel1 = '07'
    and a.gl_account <> '0062200004' and a.gl_account <> '0062200005'

and a.curtype = '10'
and a./bic/ztabla = 'R3'
and a.chrt_accts = 'SQS'

 group by 
 a.comp_code,
//  a.doc_num,
 a.co_area,
 a.chrt_accts,
 a.fiscyear, 
 a.calmonth, 
 a./bic/ztabla,
 a.curtype,
a.profit_ctr,
a.gl_account,
g.coorder,
g./bic/zmar
