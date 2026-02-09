@AbapCatalog.sqlViewName: 'ZSD_COSTOS_FI_N'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Costos de Materiales para facturas SD'
@Analytics:{ dataCategory:#CUBE , dataExtraction.enabled: true }


define view ZSD_COSTOS_FI_VIEW_N as select from /bic/azsal_a032 as a 
        inner join ZFSD_MOV_FIGL_CONFIG_N as b
            on a.bill_num  = b.bill_num and 
               a.material = b.material
                left outer join ZSD_ITEMS_FACT as c
                        on c.bill_num = a.bill_num and 
                           c.material = a.material
        
{    
    key a.bill_num ,key  a.bill_item, key a.material, 
    a.doc_number,  
    case b.ac_doc_typ
        when 'WL' then 'WL'
        when 'DR' then 'DR'
        when 'DQ' then 'DR'
        when 'DG' then 'DR'
        when 'DI' then 'DR'
        when 'DJ' then 'DR'
        when 'DK' then 'DR'
        when 'DM' then 'DR'
        when 'DN' then 'DR'
        when 'DO' then 'DR'
        when 'DP' then 'DR'
        when 'DS' then 'DR'          
    end as ac_doc_typ,     
    sum( b.Total ) as MontoUSD,
    max( c.Cantidad) as CantidadItems
    
} 
group by a.bill_num , 
         a.bill_item,  
         a.material, 
         a.doc_number,  
         b.ac_doc_typ

