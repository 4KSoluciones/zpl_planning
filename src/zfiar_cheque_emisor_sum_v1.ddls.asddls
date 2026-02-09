@AbapCatalog.sqlViewName: 'ZFIAR_CHEQUES_06'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Suma Facturas compensadas con Cheques'
define view ZFIAR_CHEQUE_EMISOR_SUM_V1 ( comp_code, debitor, clr_doc_no, deb_cre_dc, deb_cre_lc)
as select from ZFIAR_CHEQUE_EMISOR_UNION as a
{
    key a.comp_code,
    key a.debitor,
//    key a.ac_doc_typ,
    key a.clr_doc_no,
   
    @DefaultAggregation: #SUM
    sum( a.deb_cre_dc ) as DEB_CRE_DC,
    @DefaultAggregation: #SUM
    sum( a.deb_cre_lc ) as DEB_CRE_LC
} group by a.comp_code, a.debitor, a.clr_doc_no
