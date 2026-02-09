@AbapCatalog.sqlViewName: 'ZFIAR_REF_43'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'FIAR - Refinanciamiento - Facturas compensadas por RF'
define view ZFIAR_REF_ITEM_FC 
as
select from /bic/azfiar_o3n2 as c
inner join ztvarv_be as b
on  c.post_key   = b.low
and c.ac_doc_typ = b.high 
 
{
         c.comp_code, 
         c.debitor, 
         
        //ITEM FACTURA
        c.ref_key1,
        c.netduedate,
        c.doc_number, 
        c.clr_doc_no,
        c.ac_doc_typ,
        c.deb_cre_lc,
        c.deb_cre_dc 
}
where b.name = 'FC_REF' 
//and  c.clr_doc_no = '0500012620'
