*&---------------------------------------------------------------------*
*&  Include           ZFIAR_O37_END_V2 - Cheque Emisor
*&---------------------------------------------------------------------*
   TYPES: BEGIN OF ty_wa_facturas.
       INCLUDE TYPE zfiar_facturas_x_cheques.
   TYPES: used(1) TYPE c,
             END OF ty_wa_facturas.


   DATA:      lt_chxfc TYPE SORTED TABLE OF ty_wa_facturas
WITH NON-UNIQUE KEY fiscper debitor ac_doc_no netduedate  doc_number ,
wa_result       TYPE _ty_s_tg_1,
wa_result_back  TYPE _ty_s_tg_1,
lt_result TYPE _ty_t_tg_1,
lt_result_aux TYPE _ty_t_tg_1,
l_cont    TYPE i,
l_deb_cre_dc TYPE _ty_s_tg_1-deb_cre_dc,
l_deb_cre_lc TYPE _ty_s_tg_1-deb_cre_lc,
l_saldo_lc   TYPE _ty_s_tg_1-deb_cre_lc,
l_saldo_dc   TYPE _ty_s_tg_1-deb_cre_dc,
l_coef TYPE p DECIMALS 6,
l_tabix LIKE sy-tabix,
wa_facturas TYPE ty_wa_facturas.

   IF sy-batch IS INITIAL.
     BREAK-POINT.
   ENDIF.


*Orden de Facturas (cuotas):
*1)  El primer orden se realiza según la fecha de vencimiento de todas las cuotas, de menor a mayor.
*2)  En el caso de fechas de vencimiento iguales, el orden se realiza según la fecha o nro. de factura.
*3)  En el caso de facturas iguales con vencimientos iguales, se podría ordenar por nro. de cuota.
*4)  Verificar si hay casos en que los 3 anteriores coincidan y definir más criterios de ordenación.

   CHECK result_package[] IS NOT INITIAL.
   SELECT *
     INTO TABLE @DATA(lt_facturas)
     FROM zfiar_facturas_x_cheques AS a
     FOR ALL ENTRIES IN @result_package
     WHERE a~fiscper EQ @result_package-fiscper
     AND   a~debitor EQ @result_package-debitor
     AND   a~ac_doc_no EQ @result_package-ac_doc_no
     .

   IF sy-subrc EQ 0.
     lt_chxfc = CORRESPONDING #( lt_facturas ).
*DELETE ADJACENT DUPLICATES FROM lt_chxfc COMPARING  fiscper debitor ac_doc_no netduedate  doc_number.

   ENDIF.


*MAIN LOOP
**********************************************************************
*I) Orde de aplicación de los cheques:
* 1)  Primer orden por fecha de vencimiento del cheque.
* 2)  En el caso de cheques con misma fecha de vencimiento, ordenar por nro. de cheque.
    sort result_package by comp_code debitor netduedate ASCENDING ac_doc_no.

   LOOP AT result_package ASSIGNING <result_fields>."Recorro los cheques
     CLEAR wa_result.
     MOVE-CORRESPONDING  <result_fields> TO wa_result.



     AT NEW ac_doc_no.
       CLEAR: l_saldo_lc, l_saldo_dc.

     ENDAT.
     ADD:   <result_fields>-deb_cre_dc TO l_saldo_dc,
                <result_fields>-deb_cre_lc TO l_saldo_lc.


     LOOP AT lt_chxfc INTO wa_facturas WHERE fiscper = wa_result-fiscper
                                      AND   debitor =  wa_result-debitor
                                   AND   ac_doc_no = wa_result-ac_doc_no "Número Factura
                                 AND   used = ''. "Factura no compensada
*                           item_num  EQ wa_result-item_num.

       l_tabix = sy-tabix.

IF  l_saldo_lc GE wa_facturas-montofactura AND wa_facturas-montofactura GT 0.


         wa_result-ref_key1 = wa_facturas-ref_key1.
         wa_result-doc_number = wa_facturas-doc_number.
*       l_coef = ( <fs_cheques>-parccheque / <fs_cheques>-totalcheque ).

wa_result-deb_cre_dc = <result_fields>-deb_cre_dc. "Guarda monto aplicado a la factura
         wa_result-deb_cre_lc =  <result_fields>-deb_cre_lc.

*/bic/zrc_dc_lc es  finalmente usado en el reporte:
wa_result-/bic/zrc_dc_lc = wa_facturas-montofactura. "Guarda de la Factura
*      wa_result-/bic/zrc_dc_dc = l_saldo_dc.



*       Dias de diferencia:
         wa_result-/bic/znetdudat = wa_facturas-netduedate.
         wa_result-/bic/zndifvenc =  <result_fields>-netduedate -
                                        wa_facturas-netduedate.

         IF wa_result-/bic/zndifvenc GE '0' AND wa_result-/bic/zndifvenc
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
       source_record = <result_fields>-record
     IMPORTING
       record_new    = wa_result-record.

*       ADD wa_result-/bic/zrc_dc_dc TO l_total_dc.
*       ADD wa_result-/bic/zrc_dc_lc TO l_total_lc.
*

         wa_facturas-used = 'X'.

         MODIFY lt_chxfc FROM wa_facturas INDEX l_tabix.
         APPEND wa_result TO lt_result_aux[].


**********************************************************************
* Si l_saldo_lc gt wa_result-deb_cre_lc, es que la FC debe utilizar
* parte del cheque anterior y cheque actual


IF l_saldo_lc GT wa_result_back-deb_cre_lc AND wa_result_back IS NOT INITIAL.
wa_result_back-/bic/zrc_dc_lc = l_saldo_lc - wa_result_back-deb_cre_lc. "Utilizo saldo FC anterior
           wa_result_back-ref_key1 = wa_facturas-ref_key1.
           wa_result_back-doc_number = wa_facturas-doc_number.
           wa_result_back-/bic/znetdudat  = wa_result-/bic/znetdudat.
           wa_result_back-/bic/zndifvenc  = wa_result-/bic/zndifvenc.
           wa_result_back-/bic/zdifvenc   =  wa_result-/bic/zdifvenc.

           CALL METHOD me->new_record__end_routine
           EXPORTING
            source_segid  = 1
            source_record = <result_fields>-record
          IMPORTING
            record_new    = wa_result_back-record.

           APPEND wa_result_back TO lt_result_aux.
         ENDIF.
**********************************************************************

         wa_result_back = wa_result.
         l_saldo_lc = l_saldo_lc - wa_facturas-montofactura.

       ELSE.
         EXIT.

       ENDIF.

     ENDLOOP.
   ENDLOOP.
** Controla que el total del cheque no exceda la realizar el split
*       AT END OF item_num.
*
*         IF l_total_lc GT l_deb_cre_lc.
*    wa_result-/bic/zrc_dc_dc = wa_result-/bic/zrc_dc_dc - ( l_total_dc -
*    l_deb_cre_dc ).
*    wa_result-/bic/zrc_dc_lc = wa_result-/bic/zrc_dc_lc - ( l_total_lc -
*    l_deb_cre_lc ).
*           wa_result-deb_cre_dc = l_deb_cre_dc.
*           wa_result-deb_cre_lc = l_deb_cre_lc.
*
*         ELSEIF l_total_lc LT l_deb_cre_lc.
*  wa_result-/bic/zrc_dc_dc = wa_result-/bic/zrc_dc_dc + ( l_deb_cre_dc -
*  l_total_dc ).
*  wa_result-/bic/zrc_dc_lc = wa_result-/bic/zrc_dc_lc + ( l_deb_cre_lc -
*  l_total_lc ).
*           wa_result-deb_cre_dc = l_deb_cre_dc.
*           wa_result-deb_cre_lc = l_deb_cre_lc.
*
*         ENDIF.
*       ENDAT.



*     ELSE.
*
*       wa_result-/bic/zrc_dc_dc = l_deb_cre_dc.
*       wa_result-/bic/zrc_dc_lc = l_deb_cre_lc.
*
*       CALL METHOD me->new_record__end_routine
*    EXPORTING
*      source_segid  = 1
*      source_record = <result_fields>-record
*    IMPORTING
*      record_new    = wa_result-record.
*
*       APPEND wa_result TO lt_result_aux[].
*
*     ENDIF.






   result_package[] = lt_result_aux[].
