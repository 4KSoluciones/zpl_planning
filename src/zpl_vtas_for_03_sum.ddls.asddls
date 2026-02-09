@AbapCatalog.sqlViewName: 'ZPL_VTAF_3S'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Ventas Forecast Sucursal'
define view ZPL_VTAS_FOR_03_SUM with parameters i_datum : abap.dats
 as select from ZPL_VTAS_FOR_01 ( p_datum : $parameters.i_datum ) as a
{
     key a.version,
     key a.comp_code,
     key a.co_area,
     key a.fiscper,
//     key a.Linea,
     key a.Sucursal,
//     key a.Marca,
//     key a.Tipo,
     sum( a. Cant  ) as CantTotal,
     sum( a.Gastos ) as GastosTotal,
     sum( a.Ventas ) as VentasTotal
    
   
}
group by
a.version, 
a.comp_code,
a.co_area,
//a.Linea,
a.Sucursal,
a.fiscper
//a.Marca,
//a.Tipo
