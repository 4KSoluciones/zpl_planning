@AbapCatalog.sqlViewName: 'ZSD_MOV_FI_C_N'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Costos USD - Movimientos filtrados por cuenta'

define view ZFSD_MOV_FIGL_CONFIG_N as select from /bic/azfgl_a012 as b

inner join ztvarv_be as c
            on  c.low  = b.post_key
            and c.high = b.gl_account
            and c.stream = b.ac_doc_typ

{
key b.bill_num, 
key b.material,
b.ac_doc_typ, 
b.gl_account,
sum( b./bic/zfi_ml2 ) as Total     
}

where ( c.name = 'ZSD_COSTOS' and c.type = 'P')  
group by b.bill_num, b.material, 
         b.ac_doc_typ, b.gl_account

