*&---------------------------------------------------------------------*
*&  Include  zfiar_o38_end - Receptor (H)
*&---------------------------------------------------------------------*

 DATA:    lt_cheques TYPE SORTED TABLE OF zfiar_cheque_receptor
          WITH NON-UNIQUE KEY fiscper debitor ac_doc_no item_num,
          wa_result    TYPE _ty_s_tg_1,
          lt_result    TYPE _ty_t_tg_1,
          lt_result_aux TYPE _ty_t_tg_1,
          l_cont    TYPE i,
          l_deb_cre_dc TYPE _ty_s_tg_1-deb_cre_dc,
          l_deb_cre_lc TYPE _ty_s_tg_1-deb_cre_lc,
          l_total      TYPE _ty_s_tg_1-deb_cre_lc,
          l_coef TYPE p DECIMALS 6.



 FIELD-SYMBOLS: <fs_cheques> TYPE  zfiar_cheque_receptor.

 IF sy-batch IS INITIAL.
   BREAK-POINT.
 ENDIF.



 CHECK result_package[] IS NOT INITIAL.

 SELECT DISTINCT *
  INTO TABLE @lt_cheques
  FROM
   zfiar_cheque_receptor AS a
  FOR ALL ENTRIES IN @result_package
    WHERE a~fiscper EQ @result_package-fiscper
    AND a~debitor EQ @result_package-debitor
    AND a~ac_doc_no EQ @result_package-ac_doc_no
    AND a~item_num  EQ @result_package-item_num.



 LOOP AT result_package ASSIGNING <result_fields>.

   MOVE-CORRESPONDING  <result_fields> TO wa_result.
   MOVE:   <result_fields>-deb_cre_dc TO l_deb_cre_dc,
          <result_fields>-deb_cre_lc TO l_deb_cre_lc.

   CLEAR: l_total.
   LOOP AT lt_cheques  ASSIGNING <fs_cheques>
                       WHERE fiscper EQ wa_result-fiscper
                       AND  debitor EQ wa_result-debitor
                       AND  ac_doc_no EQ wa_result-ac_doc_no
                       AND  item_num  EQ wa_result-item_num.

     wa_result-doc_number = <fs_cheques>-doc_number.
     l_coef = ( <fs_cheques>-parccheque / <fs_cheques>-totalcheque ).
     wa_result-/bic/zrc_dc_dc = l_deb_cre_dc * l_coef.
     wa_result-/bic/zrc_dc_lc = l_deb_cre_lc * l_coef.
     wa_result-deb_cre_dc = l_deb_cre_dc * l_coef.
     wa_result-deb_cre_lc = l_deb_cre_lc * l_coef.


wa_result-/bic/zndifvenc =   <result_fields>-netduedate - <fs_cheques>-netduedate.

IF wa_result-/bic/zndifvenc GE '1' AND wa_result-/bic/zndifvenc LE '30'.
       wa_result-/bic/zdifvenc = 1. "01-30'.
ELSEIF wa_result-/bic/zndifvenc GE '31' AND wa_result-/bic/zndifvenc LE '60'.
       wa_result-/bic/zdifvenc = 2."'31-60'.
ELSEIF wa_result-/bic/zndifvenc GE '61' AND wa_result-/bic/zndifvenc LE '90'.
       wa_result-/bic/zdifvenc = 3."'61-90'.
     ELSEIF wa_result-/bic/zndifvenc GT '90'.
       wa_result-/bic/zdifvenc = 4. "'>90'.
     ELSE..wa_result-/bic/zdifvenc = 5."'N/A'.
     ENDIF.

     ADD wa_result-/bic/zrc_dc_lc TO l_total.

     CALL METHOD me->new_record__end_routine
     EXPORTING
       source_segid  = 1
       source_record = <result_fields>-record
     IMPORTING
       record_new    = wa_result-record.

     APPEND wa_result TO lt_result_aux.

* Controla que el total del cheque no exceda la realizar el split
     AT END OF item_num.

       IF l_total NE l_deb_cre_lc.
         wa_result-/bic/zrc_dc_dc = l_deb_cre_dc.
         wa_result-/bic/zrc_dc_lc = l_deb_cre_lc.
         wa_result-deb_cre_dc = l_deb_cre_dc.
         wa_result-deb_cre_lc = l_deb_cre_lc.

         CALL METHOD me->new_record__end_routine
       EXPORTING
         source_segid  = 1
         source_record = <result_fields>-record
       IMPORTING
         record_new    = wa_result-record.

         APPEND wa_result TO lt_result.
         CLEAR wa_result.
       ELSE.
         APPEND LINES OF lt_result_aux TO lt_result.
         CLEAR: lt_result_aux[].

       ENDIF.


     ENDAT.
   ENDLOOP.

* Si no tiene split:
   IF sy-subrc NE 0.
     wa_result-/bic/zrc_dc_dc = l_deb_cre_dc.
     wa_result-/bic/zrc_dc_lc = l_deb_cre_lc.

     CALL METHOD me->new_record__end_routine
   EXPORTING
     source_segid  = 1
     source_record = <result_fields>-record
   IMPORTING
     record_new    = wa_result-record.

     APPEND wa_result TO lt_result.
     CLEAR wa_result.
   ENDIF.
 ENDLOOP.

 result_package[] = lt_result[].
