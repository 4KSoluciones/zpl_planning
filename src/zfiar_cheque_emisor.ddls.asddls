@AbapCatalog.sqlViewName: 'ZFIAR_CHEQUES_03'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'FIAR - Obtiene Facturas SD para Cheques - Emisor'
define view ZFIAR_CHEQUE_EMISOR 
//1) CHEQUE A (Partida original)
as select distinct from /bic/azfiar_o3n2 as a
inner join ztvarv_be as ztvarv
    on a.post_key = ztvarv.low
    and a.ac_doc_typ  = ztvarv.high
  
//   2) CHEQUE B: Busco si hubo canje de cheques:        
     inner join /bic/azfiar_o3n2 as b
     on  b.comp_code = a.comp_code   
     and b.debitor = a.debitor
     and b.alloc_nmbr = a.alloc_nmbr 

//      3) ITEM FACTURA: Con cheques de A y (posibles) cheques de B, busco las facturas que estos cheques cancelan:       
        inner join /bic/azfiar_o3n2 as  c 
        on  c.comp_code = b.comp_code 
        and c.debitor = b.debitor
        and c.clr_doc_no = b.ac_doc_no //DOCUMENTO DE COMPENSACIÓN FC = CHEQUE

//       4) TOTAL FACTURA: Finalmente busco el monto total de las facturas       
//         inner join zfiar_cheques_06 as d 
//          on d.comp_code = c.comp_code
//         and d.debitor = c.debitor
//         and d.clr_doc_no = c.clr_doc_no // COMP FC = COMP FC (SELECT anterior)
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
        avg(c.deb_cre_lc) as TotalCheque // TOTAL FACTURA
//      FIN TOTAL FACTURA        
       
}where  
        ztvarv.name = 'S_CHEQUE'
and      a.alloc_nmbr <> ''
and     a.sp_gl_ind <> '' 
and     c.doc_number <> ''

group by  a.fiscper, a.debitor, a.ac_doc_no, a.item_num, 
          a.post_key, a.ac_doc_typ,  a.alloc_nmbr, a.sp_gl_ind, 
           c.netduedate, c.ref_key1, c.doc_number, c.clr_doc_no, c.ac_doc_typ  
