@AbapCatalog.sqlViewName: 'ZPL_VTAF_S2_01'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Venta Forecast Soc 2000'
define view ZPL_VTAS_FOR_S2_01  with parameters p_datum : abap.dats
as select from /bic/azpl_a357 as c
 left outer join /bi0/mprofit_ctr as d
    on c.co_area = d.co_area and
    c.profit_ctr = d.profit_ctr
    left outer join /bi0/mgl_account as f
    on c.gl_account = f.gl_account
    
{
    key  c.version,
    key  c.comp_code,
    key  c.co_area,
    key  c.fiscper,
    key  d.sales_off as Sucursal,
    key  d.distr_chan as Canal,
    key  d.matl_group as Linea,
    key  d./bic/zmar as Marca,
    key  d.profit_ctr as CEBE,
    key  c.gl_account as Cuenta,    
    key  '00000' as Orden,
    key  'venta' as Tipo,   
         '     ' as TipoOrden,
      sum( c./bic/zcan_pres ) as Cant,
//      1 as Cant,  
     0 as Gastos,
     sum( c./bic/zco_val_p ) as Ventas
//   c./bic/zco_val as Ventas
}   where  d.sales_off <> ''
 and d.matl_group <> ''
 and ( d.datefrom <= $parameters.p_datum and d.dateto >= $parameters.p_datum )
and f.chrt_accts = 'SQS'
and c.curtype = '10'
and c.co_area = 'SC02'
and c./bic/zpr_hier1 <> 'INTFI'
and f./bic/zagr_cue = '0041110001'
//and c.version = '001'

 group by
 c.version,
  c.comp_code,
  c.co_area,  
 c.fiscper,
d.sales_off,
d.distr_chan,
d./bic/zmar,
d.matl_group,
d.profit_ctr,
c.gl_account
