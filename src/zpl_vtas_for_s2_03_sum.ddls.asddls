@AbapCatalog.sqlViewName: 'ZPL_VTAF_3S_SOC2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Venta Forecast Soc 2000 - Linea, Canal'
define view ZPL_VTAS_FOR_S2_03_SUM with parameters i_datum : abap.dats
as select from ZPL_VTAS_FOR_S2_01 ( p_datum : $parameters.i_datum ) as a
{
     key a.version,
     key a.comp_code,
     key a.co_area,
     key a.fiscper,
//     key a.Marca,
//     key a.Linea,
     key a.Canal,
     key a.Sucursal,
     key a.Tipo,
     sum( a. Cant  ) as CantTotal,
     sum( a.Gastos ) as GastosTotal,
     sum( a.Ventas ) as VentasTotal
    
   
}
group by
a.version, 
a.comp_code,
a.co_area,
a.fiscper,
//a.Marca,
//a.Linea,
a.Canal,
a.Sucursal,
a.Tipo
