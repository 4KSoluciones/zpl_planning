@AbapCatalog.sqlViewName: 'ZPL_CEBE_REAL'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Participacion Gastos en Ventas'
@Metadata.ignorePropagatedAnnotations: true
@AbapCatalog.preserveKey: true 

define view ZPL_CEBE_REALES with parameters  p_datum : abap.dats
as select from /bic/azpca_o01n7 as a
 inner join /bi0/mprofit_ctr as b
    on a.co_area = b.co_area and
    a.profit_ctr = b.profit_ctr 
        inner join zpl_ord_var as g
        on a.refer_doc =  g.coorder
{
key  a.comp_code,
key  a.co_area,
key   cast( Concat( Concat( substring(a.calmonth, 1, 4), '0' ),substring(a.calmonth, 5, 2)) as abap.numc(7) ) as fiscper,
key  a./bic/ztabla,
key  a.curtype,
key  b.sales_off as Sucursal,
key  b.matl_group as Linea,
key  b./bic/zmar as Marca,
key  a.profit_ctr as CEBE,
key  a.gl_account as Cuenta,
key  g.coorder as Orden,   
key  'gasto' as Tipo,   
     g./bic/zmar as TipoOrden,

     sum( a.quantity  ) as Cant,
     sum(a./bic/zco_val ) as Gastos,
     cast('0' as abap.curr( 17, 3 ))  as Ventas
}   where 
    a.gl_account  between '0060000000' and '0069999999' 
    and  b.sales_off <> ''
 and b.matl_group <> ''
 and ( b.datefrom <= $parameters.p_datum and b.dateto >= $parameters.p_datum )
//and  b./bic/zmar = 'KEN'
and a.curtype = '10'
and a./bic/ztabla = 'R3'
and a.chrt_accts = 'SQS'
//and ( g./bic/zmar = 'VAE' or g./bic/zmar = 'SEM' )

 group by 
 a.comp_code, 
 a.co_area,
 a.calmonth, 
a./bic/ztabla,
a.curtype,
b.sales_off,
b./bic/zmar,
b.matl_group,
a.profit_ctr,
a.gl_account,
g.coorder,
g./bic/zmar

union
 
select from /bic/azpca_o01n7 as c
 inner join /bi0/mprofit_ctr as d
    on c.co_area = d.co_area 
    and c.profit_ctr = d.profit_ctr
    inner join /bi0/mgl_account as f
    on c.gl_account = f.gl_account
{
    key  c.comp_code,
    key  c.co_area,
    key   cast( Concat( Concat( substring(c.calmonth, 1, 4), '0' ),substring(c.calmonth, 5, 2)) as abap.numc(7) ) as fiscper,
    key  c./bic/ztabla,
    key  c.curtype,
    key  d.sales_off as Sucursal,
    key  d.matl_group as Linea,
    key  d./bic/zmar as Marca,
    key  c.profit_ctr as CEBE,
    key  c.gl_account as Cuenta,
    key  '00000' as Orden,
    key  'venta' as Tipo,   
         '     ' as TipoOrden,
     sum( c.quantity ) as Cant,
//      1 as Cant,  
     0 as Gastos,
     sum( c./bic/zco_val ) as Ventas
//   c./bic/zco_val as Ventas
}   where  d.sales_off <> ''
 and d.matl_group <> ''
 and ( d.datefrom <= $parameters.p_datum and d.dateto >= $parameters.p_datum )
and f.chrt_accts = 'SQS'
and c.curtype = '10'
and c./bic/ztabla = 'R3'
and f./bic/zagr_cue = '0041110001'

 group by
  c.comp_code,  
  c.co_area,
 c.calmonth,
 c./bic/ztabla,
c.curtype,
d.sales_off,
d./bic/zmar,
d.matl_group,
c.profit_ctr,
c.gl_account

