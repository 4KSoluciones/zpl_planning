*&---------------------------------------------------------------------*
*&  Include           ZFIAR_O37_END - Cheques Emisor (S)
*&---------------------------------------------------------------------*
   DATA: lt_cheques TYPE SORTED TABLE OF zfiar_cheque_emisor
              WITH NON-UNIQUE KEY fiscper debitor ac_doc_no item_num,
              wa_result TYPE _ty_s_tg_1,
              lt_result TYPE _ty_t_tg_1,
              lt_result_aux TYPE _ty_t_tg_1,
              l_cont    TYPE i,
              l_deb_cre_dc TYPE _ty_s_tg_1-deb_cre_dc,
              l_deb_cre_lc TYPE _ty_s_tg_1-deb_cre_lc,
              l_total_lc   TYPE _ty_s_tg_1-deb_cre_lc,
              l_total_dc   TYPE _ty_s_tg_1-deb_cre_dc,
              l_coef TYPE p DECIMALS 6.


   FIELD-SYMBOLS: <fs_cheques> TYPE zfiar_cheque_emisor.

   IF sy-batch IS INITIAL.
     BREAK-POINT.
   ENDIF.



   CHECK result_package[] IS NOT INITIAL.
   SELECT *
     INTO TABLE @lt_cheques
     FROM zfiar_cheque_emisor
     FOR ALL ENTRIES IN @result_package
     WHERE fiscper EQ @result_package-fiscper
     AND   debitor EQ @result_package-debitor
     AND   ac_doc_no EQ @result_package-ac_doc_no
     AND  item_num   EQ @result_package-item_num.



   LOOP AT result_package ASSIGNING <result_fields>.
     MOVE-CORRESPONDING  <result_fields> TO wa_result.
     MOVE:   <result_fields>-deb_cre_dc TO l_deb_cre_dc,
            <result_fields>-deb_cre_lc TO l_deb_cre_lc.

     CLEAR: l_total_dc, l_total_lc.
     LOOP AT lt_cheques  ASSIGNING <fs_cheques>
                         WHERE fiscper EQ wa_result-fiscper
                         AND  debitor EQ wa_result-debitor
                         AND  ac_doc_no EQ wa_result-ac_doc_no
                         AND  item_num  EQ wa_result-item_num.
*       add 1 to g_cont.
*       wa_result-record = g_cont.

       wa_result-ref_key1 = <fs_cheques>-ref_key1.
       wa_result-doc_number = <fs_cheques>-doc_number.
       l_coef = ( <fs_cheques>-parccheque / <fs_cheques>-totalcheque ).
       wa_result-/bic/zrc_dc_dc = l_deb_cre_dc * l_coef.
       wa_result-/bic/zrc_dc_lc = l_deb_cre_lc * l_coef.
       wa_result-deb_cre_dc = l_deb_cre_dc * l_coef.
       wa_result-deb_cre_lc = l_deb_cre_lc * l_coef.

       wa_result-/bic/znetdudat = <fs_cheques>-netduedate.
       wa_result-/bic/zndifvenc =  <result_fields>-netduedate - <fs_cheques>-netduedate.

       if wa_result-/bic/zndifvenc ge '0' and wa_result-/bic/zndifvenc le '30'.
        wa_result-/bic/zdifvenc = 1. "01-30'.
       elseif wa_result-/bic/zndifvenc ge '31' and wa_result-/bic/zndifvenc le '60'.
        wa_result-/bic/zdifvenc = 2."'31-60'.
       elseif wa_result-/bic/zndifvenc ge '61' and wa_result-/bic/zndifvenc le '90'.
        wa_result-/bic/zdifvenc = 3."'61-90'.
       elseif wa_result-/bic/zndifvenc gt '90'.
        wa_result-/bic/zdifvenc = 4. "'>90'.
       else..wa_result-/bic/zdifvenc = 5."'N/A'.
      endif.

       CALL METHOD me->new_record__end_routine
   EXPORTING
     source_segid  = 1
     source_record = <result_fields>-record
   IMPORTING
     record_new    = wa_result-record.

       ADD wa_result-/bic/zrc_dc_dc TO l_total_dc.
       ADD wa_result-/bic/zrc_dc_lc TO l_total_lc.

* Controla que el total del cheque no exceda la realizar el split
       AT END OF item_num.

         IF l_total_lc GT l_deb_cre_lc.
wa_result-/bic/zrc_dc_dc = wa_result-/bic/zrc_dc_dc - ( l_total_dc - l_deb_cre_dc ).
wa_result-/bic/zrc_dc_lc = wa_result-/bic/zrc_dc_lc - ( l_total_lc - l_deb_cre_lc ).
           wa_result-deb_cre_dc = l_deb_cre_dc.
           wa_result-deb_cre_lc = l_deb_cre_lc.

         ELSEIF l_total_lc LT l_deb_cre_lc.
wa_result-/bic/zrc_dc_dc = wa_result-/bic/zrc_dc_dc + ( l_deb_cre_dc - l_total_dc ).
wa_result-/bic/zrc_dc_lc = wa_result-/bic/zrc_dc_lc + ( l_deb_cre_lc - l_total_lc ).
           wa_result-deb_cre_dc = l_deb_cre_dc.
           wa_result-deb_cre_lc = l_deb_cre_lc.

         ENDIF.
       ENDAT.

       APPEND wa_result TO lt_result_aux[].

     ENDLOOP.
     IF sy-subrc EQ 4.
       wa_result-/bic/zrc_dc_dc = l_deb_cre_dc.
       wa_result-/bic/zrc_dc_lc = l_deb_cre_lc.

       CALL METHOD me->new_record__end_routine
    EXPORTING
      source_segid  = 1
      source_record = <result_fields>-record
    IMPORTING
      record_new    = wa_result-record.

       APPEND wa_result TO lt_result_aux[].

     ENDIF.
   ENDLOOP.


   result_package[] = lt_result_aux[].
