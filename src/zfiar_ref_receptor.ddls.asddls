@AbapCatalog.sqlViewName: 'ZFIAR_REF_41'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Refinanciamiento - Receptor'
// 1) Selecciono los documentos de Refinanciación de la tabla ZTVARV
define view ZFIAR_REF_RECEPTOR  as select from /bic/azfiar_o3n7 as a
inner join ztvarv_be as ztvarv
    on a.post_key = ztvarv.low
    and a.ac_doc_typ  = ztvarv.high

//2) Busco las Facturas compensadas por las ref. paso 1
inner  join /bic/azfiar_o3n7 as b
    on  b.comp_code = a.comp_code
    and b.debitor= a.debitor 
    and b.ac_doc_no <> a.clr_doc_no   
    and b.clr_doc_no = a.clr_doc_no
    and b.doc_number <> ''  

//3) Busco el total de los documentos Factura para prorratear los Ref de 1)        
          inner join  zfiar_ref_40 as d 
          on  d.comp_code = b.comp_code
          and d.debitor = b.debitor
         and d.clr_doc_no = b.clr_doc_no
//         and d.ac_doc_typ = a.ac_doc_typ
         
        {
        key a.fiscper, 
        key a.debitor, 
        key a.ac_doc_no,   
        key a.item_num,
        b.doc_number as doc_number,
//        a.post_key, a.ac_doc_typ,  a.alloc_nmbr, a.sp_gl_ind,
//        
//        b.ac_doc_no as NumCheque, 
//        b.ac_doc_typ as tipoCheque, 
//        b.clr_doc_no as CompCheque,
//        
//        b.ac_doc_typ as TipoFac,
//        b.ac_doc_no as NumFac,
//        b.clr_doc_no as compFac,
//        
//        c.deb_cre_dc as MontoCheque,
//        d.deb_cre_dc as MontoTotalFactura 
        @DefaultAggregation: #SUM
        sum(b.deb_cre_lc) as ParcCheque,
        max(d.deb_cre_lc) as TotalCheque 
}where     ztvarv.name = 'H_REF'
and      a.ac_doc_no = '0500012620'
group by  a.fiscper, a.debitor, a.ac_doc_no, a.item_num, 
          b.doc_number


