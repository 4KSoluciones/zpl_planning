@AbapCatalog.sqlViewName: 'ZPL_VPRE_SL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Analytics.dataExtraction.enabled: true            // Enables ODP Extraction
@EndUserText.label: 'PL: Venta Presup X Sucursal y Linea'
define view ZPL_VTAS_PRES_SUC_LINEA 
 with parameters i_datum : abap.dats
 as select from ZPL_CEBES_PRES( p_datum : $parameters.i_datum ) as a
{
     key a.comp_code,
     key a.co_area,
//     key cast( left( a.fiscper, 4 ) as abap.numc( 4 )) as fiscyear,
     key a.fiscper,
     key a.Sucursal,
     key a.Linea,
     key a.Tabla,
//     key a.Marca,
//     key a.Tipo,
     sum( a. Cant  ) as CantTotal,
     sum( a.Gastos ) as GastosTotal,
     sum( a.Ventas ) as VentasTotal
    
   
}where a.Tipo = 'venta'
and a.Ventas <> 0
and a.Linea  <> '0' and a.Linea  <> '' and a.Linea  <> '000'
group by 
a.comp_code,
a.co_area,
a.fiscper,
a.Sucursal,
a.Linea,
a.Tabla

//a.Marca,
//a.Tipo
