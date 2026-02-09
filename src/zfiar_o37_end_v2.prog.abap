*&---------------------------------------------------------------------*
*&  Include           ZFIAR_O37_END_V2 - Cheques Emisor (S)
*&---------------------------------------------------------------------*
   TYPES: BEGIN OF ty_wa_facturas.
       INCLUDE TYPE zfiar_facturas_x_cheques.
   TYPES: used(1) TYPE c,
          END OF ty_wa_facturas.


   DATA: lt_chxfc       TYPE STANDARD  TABLE OF ty_wa_facturas,
         wa_result      TYPE _ty_s_tg_1,
         wa_result_back TYPE _ty_s_tg_1,
         lt_key         TYPE _ty_t_tg_1,
         lt_result      TYPE _ty_t_tg_1,
         lt_result_aux  TYPE _ty_t_tg_1,
         l_cont         TYPE i,
         l_deb_cre_dc   TYPE _ty_s_tg_1-deb_cre_dc,
         l_deb_cre_lc   TYPE _ty_s_tg_1-deb_cre_lc,
         l_saldo_lc     TYPE _ty_s_tg_1-deb_cre_lc,
         l_saldo_dc     TYPE _ty_s_tg_1-deb_cre_dc,
         l_coef         TYPE p DECIMALS 6,
         l_tabix        LIKE sy-tabix,
         wa_facturas    TYPE ty_wa_facturas.

   IF sy-batch IS INITIAL.
     BREAK-POINT.
   ENDIF.

   SORT result_package BY comp_code debitor ac_doc_no fi_docstat ASCENDING netduedate ASCENDING alloc_nmbr ASCENDING.
   lt_key =  CORRESPONDING #( result_package ).
   SORT lt_key BY comp_code debitor ac_doc_no.
   DELETE ADJACENT DUPLICATES FROM lt_key COMPARING comp_code debitor ac_doc_no.

*Orden de Facturas (cuotas):
*1)  El primer orden se realiza según la fecha de vencimiento de todas las cuotas, de menor a mayor.
*2)  En el caso de fechas de vencimiento iguales, el orden se realiza según la fecha o nro. de factura.
*3)  En el caso de facturas iguales con vencimientos iguales, se podría ordenar por nro. de cuota.
*4)  Verificar si hay casos en que los 3 anteriores coincidan y definir más criterios de ordenación.

   CHECK lt_key[] IS NOT INITIAL.
   SELECT *
     INTO TABLE @DATA(lt_facturas)
     FROM zfiar_facturas_x_cheques AS a
     FOR ALL ENTRIES IN @lt_key
     WHERE a~fiscper EQ @lt_key-fiscper
     AND   a~debitor EQ @lt_key-debitor
     AND   a~ac_doc_no EQ @lt_key-ac_doc_no
     AND   a~montofactura GT 0.



   IF sy-subrc EQ 0.


     lt_chxfc = CORRESPONDING #( lt_facturas ).
     SORT lt_chxfc BY  fiscper debitor ac_doc_no ref_key1 ASCENDING netduedate ASCENDING doc_number ASCENDING.

   ENDIF.


*MAIN LOOP
**********************************************************************
*I) Orden de aplicación de los cheques:
* 1)  Primer orden por fecha de vencimiento del cheque.
* 2)  En el caso de cheques con misma fecha de vencimiento, ordenar por nro. de cheque.



*Loopeo en cada cheque (AC_DOC_NO):
   LOOP AT lt_key ASSIGNING <result_fields>.


     CLEAR: l_saldo_lc, l_saldo_dc.

*     lt_result_aux = filter #( result_package WHERE  fiscper   =  <result_fields>-fiscper
*                                                              AND    debitor   = <result_fields>-debitor
*                                                              AND    ac_doc_no = <result_fields>-ac_doc_no  ).

*    Recorro posiciones de Cheque (item 10, 20, NN):
     LOOP AT result_package ASSIGNING FIELD-SYMBOL(<fs_cheque>) WHERE  fiscper   =  <result_fields>-fiscper
                                                              AND    debitor   = <result_fields>-debitor
                                                              AND    ac_doc_no = <result_fields>-ac_doc_no.



       CLEAR wa_result.
       MOVE-CORRESPONDING  <fs_cheque> TO wa_result.

       l_saldo_lc = l_saldo_lc + wa_result-deb_cre_lc.



       LOOP AT lt_chxfc INTO wa_facturas WHERE fiscper = <fs_cheque>-fiscper
                                           AND debitor = <fs_cheque>-debitor
                       AND ac_doc_no = <fs_cheque>-ac_doc_no. "Número Cheque


         l_tabix = sy-tabix.

         CHECK wa_facturas-used IS INITIAL.


         IF   l_saldo_lc GE wa_facturas-montofactura.

           wa_facturas-used = 'X'."Se compensa el total de la FC
           l_saldo_lc = l_saldo_lc - wa_facturas-montofactura.
*       CUIDADO: /bic/zrc_dc_lc es  finalmente usado en el reporte:
           wa_result-/bic/zrc_dc_lc = wa_facturas-montofactura. "Guarda de la Factura


         ELSEIF   l_saldo_lc GT 0."Consume primero este saldo, que es remanente de Cheque anterior
           wa_facturas-used = ''.
           wa_result-/bic/zrc_dc_lc = l_saldo_lc. "Saldo de la FC
           wa_facturas-montofactura = wa_facturas-montofactura - l_saldo_lc.
           l_saldo_lc = 0.

         ELSE.

           EXIT."Próximo cheque
         ENDIF.

*********Modificado el 21/05/2024 para traer el canal de distribución desde la factura
         wa_result-distr_chan = wa_facturas-distr_chan.
*********FIN modifcacion 21/05/2024

         wa_result-ref_key1 = wa_facturas-ref_key1.
         wa_result-doc_number = wa_facturas-doc_number.

         wa_result-deb_cre_dc = <fs_cheque>-deb_cre_dc. "Guarda monto aplicado a la factura
         wa_result-deb_cre_lc =  <fs_cheque>-deb_cre_lc.

*       Dias de diferencia:
         wa_result-/bic/znetdudat = wa_facturas-netduedate.
         wa_result-/bic/zndifvenc =  <fs_cheque>-netduedate -
                                        wa_facturas-netduedate."Fecha Venc Cuota N

         IF wa_result-/bic/zndifvenc GE '1' AND wa_result-/bic/zndifvenc
  LE '30'.
           wa_result-/bic/zdifvenc = 1. "01-30'.
         ELSEIF wa_result-/bic/zndifvenc GE '31' AND
  wa_result-/bic/zndifvenc LE '60'.
           wa_result-/bic/zdifvenc = 2."'31-60'.
         ELSEIF wa_result-/bic/zndifvenc GE '61' AND
  wa_result-/bic/zndifvenc LE '90'.
           wa_result-/bic/zdifvenc = 3."'61-90'.
         ELSEIF wa_result-/bic/zndifvenc GT '90'.
           wa_result-/bic/zdifvenc = 4. "'>90'.
         ELSE..wa_result-/bic/zdifvenc = 5."'N/A'.
         ENDIF.

         CALL METHOD me->new_record__end_routine
           EXPORTING
             source_segid  = 1
             source_record = <fs_cheque>-record
           IMPORTING
             record_new    = wa_result-record.

*



         MODIFY lt_chxfc FROM wa_facturas INDEX l_tabix.
         APPEND wa_result TO lt_result_aux[].



       ENDLOOP. "Facturas
     ENDLOOP."Members: Cheques
   ENDLOOP."Groups: fiscper, debitor, ac_doc_no





   result_package[] = lt_result_aux[].
