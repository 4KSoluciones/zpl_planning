@AbapCatalog.sqlViewName: 'ZPL_VTA_01'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Venta Real Nivel CEBE'
define view ZPL_VTA_REAL_CEBE 
with parameters i_datum : abap.dats
 as select from ZPL_CEBE_REALES( p_datum : $parameters.i_datum ) as a
{
     key a.comp_code,
     key a.co_area,
     key cast( left( a.fiscper, 4 ) as abap.numc( 4 )) as fiscyear,
     key a.fiscper,
     key a./bic/ztabla,
     key a.curtype,
     key a.CEBE,
//     key a.Tipo,
     sum( a.Cant )   as Canttotal,
     sum( a.Gastos ) as GastosTotal,
     sum( a.Ventas ) as VentasTotal
    
   
} 
where a.Marca <> '0' and a.Marca <> '' and a.Marca <> '000'
and a.Tipo = 'venta'
group by 
a.comp_code,
a.co_area,
a.fiscper,
a./bic/ztabla,
a.curtype,
a.CEBE;
