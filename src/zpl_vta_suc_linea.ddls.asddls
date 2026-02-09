@AbapCatalog.sqlViewName: 'ZPL_VTA_SUC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Venta Real por Sucursal y Linea'
define view ZPL_VTA_SUC_LINEA 
with parameters i_datum : abap.dats
 as select from ZPL_CEBE_REALES( p_datum : $parameters.i_datum ) as a
{

     key a.comp_code,
     key a.co_area,
//     key a.CEBE,

//     key cast( left( a.fiscper, 4 ) as abap.numc( 4 )) as fiscyear,
     key a.fiscper,
//     key a./bic/ztabla,
//     key a.curtype,
     key a.Sucursal,
     key a.Linea,

//     key a.Marca,
//     key a.Tipo,
     sum( a.Cant )   as Canttotal,
     sum( a.Gastos ) as GastosTotal,
     sum( a.Ventas ) as VentasTotal
   
   
}
where a.Tipo = 'venta'
and a.Ventas <> 0
and a.Linea  <> '0' and a.Linea  <> '' and a.Linea  <> '000'
group by 
a.comp_code,
a.co_area,
//a.CEBE,
a.fiscper,
//a./bic/ztabla,
//a.curtype,
a.Sucursal,
a.Linea,
//a.Marca,
a.Tipo
