@AbapCatalog.sqlViewName: 'ZFIAR_CHEQUES_02'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'FIAR - Obtiene Facturas SD para Cheques - Receptor'
// 1) Selecciono los cheques de la tabla ZTVARV
define view ZFIAR_CHEQUE_RECEPTOR as select from /bic/azfiar_o3n7 as a
inner join ztvarv_be as ztvarv
    on a.post_key = ztvarv.low
    and a.ac_doc_typ  = ztvarv.high

//2) Busco los cheques canjeados / compensados por cheques paso 1
inner  join /bic/azfiar_o3n7 as b
    on  b.comp_code = a.comp_code
    and b.debitor= a.debitor 
    and b.ac_doc_no <> a.clr_doc_no   
    and b.clr_doc_no = a.clr_doc_no
    and b.sp_gl_ind <> ''

//3) Busco los documentos que fueron compensados por cheques paso 2
//Deberían ser facturas que ya contengan referencia a Factura SD         
        inner join /bic/azfiar_o3n7 as  c 
        on c.comp_code = b.comp_code
        and c.debitor = b.debitor
        and c.clr_doc_no = b.ac_doc_no
        and c.doc_number <> ''  
        
//4) Busco el total de los documentos Factura para prorratear los cheques en 1)        
          inner join  zfiar_cheques_04 as d 
          on  d.comp_code = c.comp_code
          and d.debitor = c.debitor
         and d.clr_doc_no = c.clr_doc_no
         and d.ac_doc_typ = c.ac_doc_typ
         
        {
        key a.fiscper, key a.debitor, key a.ac_doc_no, key a.item_num,
        c.netduedate,
        c.doc_number as doc_number,
//        a.post_key, a.ac_doc_typ,  a.alloc_nmbr, a.sp_gl_ind,
//        
//        b.ac_doc_no as NumCheque, 
//        b.ac_doc_typ as tipoCheque, 
//        b.clr_doc_no as CompCheque,
//        
        c.ac_doc_typ as TipoFac,
        c.ac_doc_no as NumFac,
        c.clr_doc_no as compFac,
//        
//        c.deb_cre_dc as MontoCheque,
//        d.deb_cre_dc as MontoTotalFactura 
        @DefaultAggregation: #SUM
        sum(c.deb_cre_lc) as ParcCheque,
        avg(d.deb_cre_lc) as TotalCheque 
}where  a.sp_gl_ind <> ''
   and    ztvarv.name = 'H_CHEQUE'
group by  a.fiscper, a.debitor, a.ac_doc_no, a.item_num, 
          c.netduedate, c.doc_number, 
          c.ac_doc_no, c.clr_doc_no, c.ac_doc_typ 


