@AbapCatalog.sqlViewName: 'ZFIAR_REF_39'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'FIAR - Obtiene Facturas SD para Refinanciaciones - Emisor'
define view ZFIAR_REF_EMISOR 
//1) Refinanciacion A (Partida original)
as select from /bic/azfiar_o3n2 as a
inner join ztvarv_be as ztvarv
    on a.post_key = ztvarv.low
    and a.ac_doc_typ  = ztvarv.high
  
//      2) ITEM FACTURA: Con Ref de A, busco las facturas que esta ref. cancelan:       
        inner join ZFIAR_REF_ITEM_FC as  c 
        on  c.comp_code = a.comp_code 
        and c.debitor = a.debitor
        and c.clr_doc_no = a.ac_doc_no //DOCUMENTO DE COMPENSACIÓN FC = Ref

//       4) TOTAL FACTURA: Finalmente busco el monto total de las facturas       
         inner join ZFIAR_REF_EMISOR_SUM as d 
          on d.comp_code = c.comp_code
         and d.debitor = c.debitor
         and d.clr_doc_no = c.clr_doc_no // COMP FC = COMP FC (SELECT anterior)
//         and d.ac_doc_typ = c.ac_doc_typ
         
        
        {
        key a.fiscper, key a.debitor, key a.ac_doc_no, key a.item_num,
        a.post_key, a.ac_doc_typ,  a.alloc_nmbr, a.sp_gl_ind,
//      ITEM FACTURA: 
        c.ref_key1,
        c.netduedate,
        c.doc_number, 
        c.clr_doc_no as compFac,
        c.ac_doc_typ as TipoFac,
        @DefaultAggregation: #SUM
        sum(c.deb_cre_lc) as ParcCheque,//ITEM FACTURA
//      FIN ITEM FACTURA

//      TOTAL FACTURA
        avg(d.deb_cre_lc) as TotalCheque // TOTAL FACTURA
//      FIN TOTAL FACTURA        
       
}where  
        ztvarv.name = 'S_REF'
and     c.doc_number <> ''

group by  a.fiscper, a.debitor, a.ac_doc_no, a.item_num, 
          a.post_key, a.ac_doc_typ,  a.alloc_nmbr, a.sp_gl_ind, 
           c.netduedate, c.ref_key1, c.doc_number, c.clr_doc_no, c.ac_doc_typ  
