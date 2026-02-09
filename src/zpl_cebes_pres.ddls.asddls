@AbapCatalog.sqlViewName: 'ZPL_CEBES_PRESUP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Analytics.dataExtraction.enabled: true            // Enables ODP Extraction
@EndUserText.label: 'PL: Venta Presupuesto COPA'
define view ZPL_CEBES_PRES with parameters p_datum : abap.dats
as select from /bic/azpl_a367 as c
 inner join /bi0/mcostcenter as d
    on c.co_area = d.co_area and
    c.profit_ctr = d.costcenter
    inner join /bi0/mgl_account as f
    on c.gl_account = f.gl_account
    
{
    key  c.comp_code,
    key  c.co_area,
    key  c.fiscper,
    key  c./bic/ztabla as Tabla,
    key  d.sales_off as Sucursal,
    key  d.matl_group as Linea,
    key  d./bic/zmar as Marca,
    key  d.costcenter as CEBE,
    key  c.gl_account as Cuenta,
    key  '00000' as Orden,
    key  'venta' as Tipo,   
         '     ' as TipoOrden,
      sum( c.copaslqty ) as Cant,
//      1 as Cant,  
     0 as Gastos,
     sum( c.copareven ) as Ventas
//   c./bic/zco_val as Ventas
}   where  d.sales_off <> ''
 and d.matl_group <> ''
 and d.objvers = 'A'
 and ( d.datefrom <= $parameters.p_datum and d.dateto >= $parameters.p_datum )
and f.chrt_accts = 'SQS'
and c.currency = 'PYG'
and c.version = '001'
//and c./bic/ztabla = '25V0'
and f./bic/zagr_cue = '0041110001'
and c./bic/zpr_hier1 <> 'INTFI'

 group by
  c.comp_code,
  c.co_area,  
 c.fiscper,
 c./bic/ztabla,
d.sales_off,
d./bic/zmar,
d.matl_group,
d.costcenter,
c.gl_account
