@AbapCatalog.sqlViewName: 'ZSD_MOV_FI'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Movimientos Costos y Ventas FIGL'
define view ZSD_MOVIMIENTOS_FIGL as select distinct from /bic/azdsgl_1800 as b {
key b.bill_num, 
key b.material,
b.doc_num, 
b.ac_doc_typ, 
b.post_key,
b.gl_account, 
sum( b./bic/zfi_ml2 ) as Total
    
}where (  b.gl_account = '0051110001' or b.gl_account = '0041110001' or b.gl_account = '0051110002' 
           or b.gl_account = '0041110002' or b.gl_account = '0051110005' or b.gl_account = '0042500001' or b.gl_account = '0011211021' )
  and ( b.ac_doc_typ = 'WL' or b.ac_doc_typ = 'DR' or b.ac_doc_typ = 'DQ' or b.ac_doc_typ = 'DG' or b.ac_doc_typ = 'DI'
     or b.ac_doc_typ = 'DJ' or b.ac_doc_typ = 'DK' or b.ac_doc_typ = 'DM' or b.ac_doc_typ = 'DN' or b.ac_doc_typ = 'DO' or b.ac_doc_typ = 'DP'or b.ac_doc_typ = 'DS')
  and ( b.post_key  = '81' or b.post_key = '50' or b.post_key = '40')
  group by b.bill_num, b.material, b.doc_num, b.ac_doc_typ, b.post_key, b.gl_account
