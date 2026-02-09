@AbapCatalog.sqlViewName: 'ZPL_CEBES_PRES_U'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Analytics.dataExtraction.enabled: true            // Enables ODP Extraction
@EndUserText.label: 'PL: CEBES Union Presupuesto'
define view ZPL_CEBES_UNION_PRES 
with parameters i_datum : abap.dats
 as select from ZPL_CEBES_PRES( p_datum : $parameters.i_datum ) as a
{
     key a.comp_code,
     key a.co_area,
     key cast( left( a.fiscper, 4 ) as abap.numc( 4 )) as fiscyear,
     key a.fiscper,
     key a.Sucursal,
     key a.Linea,
     key a.Marca,
     key a.Tipo,
     key a.Tabla,
     sum( a.Cant  ) as CantTotal,
     sum( a.Gastos ) as GastosTotal,
     sum( a.Ventas ) as VentasTotal
    
   
}
where a.Marca <> '0' and a.Marca <> '' and a.Marca <> '000'
group by 
a.comp_code,
a.co_area,
a.Sucursal,
a.Linea,
a.fiscper,
a.Marca,
a.Tipo,
a.Tabla
