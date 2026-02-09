@AbapCatalog.sqlViewName: 'ZPL_VTAP_21S'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Venta Presup Soc 2000 - Clave Full - Agregado'
define view ZPL_VTA_PRES_S2_01_SUM with parameters i_datum : abap.dats
 as select from ZPL_VTA_PRES_S2_01 ( p_datum : $parameters.i_datum ) as a
{
     key a.comp_code,
     key a.co_area,
     key cast( left( a.fiscper, 4 ) as abap.numc( 4 )) as fiscyear,
     key a.fiscper,
     key a.Linea,
     key a.Canal,
     key a.Sucursal,
     key a.Marca,
     key a.Tipo,
     key a.Tabla,
     sum( a. Cant  ) as CantTotal,
     sum( a.Gastos ) as GastosTotal,
     sum( a.Ventas ) as VentasTotal
    
   
}
group by 
a.comp_code,
a.co_area,
a.Linea,
a.Canal,
a.Sucursal,
a.fiscper,
a.Marca,
a.Tipo,
a.Tabla
