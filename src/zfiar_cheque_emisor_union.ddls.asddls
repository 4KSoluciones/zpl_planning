@AbapCatalog.sqlViewName: 'ZFIAR_CHEQUES_05'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Datos de Facturas y de Cheques c Pago Parcial'
define view ZFIAR_CHEQUE_EMISOR_UNION( comp_code, debitor, ac_doc_typ, clr_doc_no, deb_cre_dc, deb_cre_lc)
as
select from /bic/azfiar_o3n2 as a
inner join ztvarv_be as b
on  a.post_key   = b.low
and a.ac_doc_typ = b.high 
 
{
    key a.comp_code,
    key a.debitor,
    key a.ac_doc_typ,
    key a.clr_doc_no,
   
    @DefaultAggregation: #SUM
    sum( a.deb_cre_dc ) as DEB_CRE_DC,
    @DefaultAggregation: #SUM
    sum( a.deb_cre_lc ) as DEB_CRE_LC
}
--and a.post_key = '01'
--and  a.ac_doc_typ = 'DR'
where b.name = 'FC_CHEQUE' 
--and  a.clr_doc_no = '6301299186'
group by a.comp_code, a.debitor, a.ac_doc_typ, a.clr_doc_no

union 
select from /bic/azfiar_o3n2 as c
inner join ztvarv_be as e
on  c.post_key   = e.low
and c.ac_doc_typ = e.high 
and c.inv_doc_no <> ''
 
{
    key c.comp_code,
    key c.debitor,
    key c.ac_doc_typ,
    key c.ac_doc_no as clr_doc_no,
   
    @DefaultAggregation: #SUM
    abs( c.deb_cre_lc ) as DEB_CRE_DC,
    @DefaultAggregation: #SUM
    abs( c.deb_cre_lc ) as DEB_CRE_LC
}
--and a.post_key = '01'
--and  a.ac_doc_typ = 'DR'
//and a.clr_doc_no = '6301344122'
where e.name = 'H' 
--and  c.ac_doc_no = '6301299186' 
