@AbapCatalog.sqlViewName: 'ZPL_VTAP_23S'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Venta Presup Soc 2000 - Suc, Canal'
define view ZPL_VTA_PRES_S2_03_SUM with parameters i_datum : abap.dats
 as select from ZPL_VTA_PRES_S2_01 ( p_datum : $parameters.i_datum ) as a
{
     key a.comp_code,
     key a.co_area,
//     key cast( left( a.fiscper, 4 ) as abap.numc( 4 )) as fiscyear,
     key a.fiscper,
     key a.Sucursal,
     key a.Canal,
     key a.Tabla,
//     key a.Linea,
//     key a.Marca,
//     key a.Tipo,
     sum( a. Cant  ) as CantTotal,
     sum( a.Gastos ) as GastosTotal,
     sum( a.Ventas ) as VentasTotal
    
   
}where a.Tipo = 'venta'
and a.Ventas <> 0
group by 
a.comp_code,
a.co_area,
a.fiscper,


a.Sucursal,
a.Canal,
a.Tabla
//a.Linea
//a.Marca,
//a.Tipo
