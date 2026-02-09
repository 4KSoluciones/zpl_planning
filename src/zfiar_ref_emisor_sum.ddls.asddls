@AbapCatalog.sqlViewName: 'ZFIAR_REF_40'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Suma Facturas compensadas con Refinanciación'
define view ZFIAR_REF_EMISOR_SUM ( comp_code, debitor, clr_doc_no,  deb_cre_dc, deb_cre_lc)
as
select from /bic/azfiar_o3n2 as a
inner join ztvarv_be as b
on  a.post_key   = b.low
and a.ac_doc_typ = b.high 
 
{
    key a.comp_code,
    key a.debitor,
//    key a.ac_doc_typ,
    key a.clr_doc_no,
   
    @DefaultAggregation: #SUM
    sum( a.deb_cre_dc ) as DEB_CRE_DC,
    @DefaultAggregation: #SUM
    sum( a.deb_cre_lc ) as DEB_CRE_LC
}
where b.name = 'FC_REF' 
and  a.clr_doc_no = '0500012620'
group by a.comp_code, a.debitor, a.clr_doc_no

