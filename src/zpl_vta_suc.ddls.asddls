@AbapCatalog.sqlViewName: 'ZPL_VTA_SUC_V'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Venta Real por Sucursal'
define view ZPL_VTA_SUC_ 
with parameters i_datum : abap.dats
 as select from ZPL_CEBE_REALES( p_datum : $parameters.i_datum ) as a
{
     key a.comp_code,
     key a.co_area,
//     key cast( left( a.fiscper, 4 ) as abap.numc( 4 )) as fiscyear,
     key a.fiscper,
//     key a./bic/ztabla,
//     key a.curtype,
//     key a.Linea,
     key a.Sucursal,
//     key a.Marca,
//     key a.Tipo,
     sum( a.Cant )   as Canttotal,
     sum( a.Gastos ) as GastosTotal,
     sum( a.Ventas ) as VentasTotal
   
   
}
where a.Tipo = 'venta'
and   a.Ventas <> 0 
and   a.Sucursal  <> '0' and a.Sucursal  <> '' and a.Sucursal  <> '000'
group by 
a.comp_code,
a.co_area,
a.fiscper,
//a./bic/ztabla,
//a.curtype,
//a.Linea,
a.Sucursal
//a.Marca,
//a.Tipo
