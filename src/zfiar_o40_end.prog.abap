*&---------------------------------------------------------------------*
*&  Include  zfiar_o40_end - Receptor (H)
*&---------------------------------------------------------------------*

 DATA:    lt_refs TYPE SORTED TABLE OF zfiar_ref_receptor
          WITH NON-UNIQUE KEY fiscper debitor ac_doc_no item_num,
          wa_result    TYPE _ty_s_tg_1,
          lt_result    TYPE _ty_t_tg_1,
          lt_result_aux TYPE _ty_t_tg_1,
          l_cont    TYPE i,
          l_deb_cre_dc TYPE _ty_s_tg_1-deb_cre_dc,
          l_deb_cre_lc TYPE _ty_s_tg_1-deb_cre_lc,
          l_total      TYPE _ty_s_tg_1-deb_cre_lc,
          l_total_lc   TYPE _ty_s_tg_1-deb_cre_lc,
          l_total_dc   TYPE _ty_s_tg_1-deb_cre_dc,
          l_coef TYPE p DECIMALS 6.





 IF sy-batch IS INITIAL.
   BREAK-POINT.
 ENDIF.



 CHECK result_package[] IS NOT INITIAL.

 SELECT DISTINCT *
  INTO TABLE @lt_refs
  FROM
   zfiar_ref_receptor AS a
  FOR ALL ENTRIES IN @result_package
    WHERE a~fiscper EQ @result_package-fiscper
    AND a~debitor EQ @result_package-debitor
    AND a~ac_doc_no EQ @result_package-ac_doc_no
        AND a~item_num EQ @result_package-item_num.




 LOOP AT result_package ASSIGNING <result_fields>.

   MOVE-CORRESPONDING  <result_fields> TO wa_result.
   MOVE:   <result_fields>-deb_cre_dc TO l_deb_cre_dc,
          <result_fields>-deb_cre_lc TO l_deb_cre_lc.

   CLEAR: l_total.
   LOOP AT lt_refs  INTO DATA(wa_refs)
                       WHERE fiscper EQ wa_result-fiscper
                       AND  debitor EQ wa_result-debitor
                       AND  ac_doc_no EQ wa_result-ac_doc_no
                       AND  item_num EQ wa_result-item_num.


     wa_result-doc_number = wa_refs-doc_number.
*     wa_result-/bic/znetdudat = <fs_refs>-netduedate.
*     wa_result-ref_key1 = <fs_refs>-ref_key1.
     l_coef = ( wa_refs-parccheque / wa_refs-totalcheque ).
     wa_result-/bic/zrc_dc_dc = l_deb_cre_dc * l_coef.
     wa_result-/bic/zrc_dc_lc = l_deb_cre_lc * l_coef.
     wa_result-deb_cre_dc = l_deb_cre_dc * l_coef.
     wa_result-deb_cre_lc = l_deb_cre_lc * l_coef.



     CALL METHOD me->new_record__end_routine
     EXPORTING
       source_segid  = 1
       source_record = <result_fields>-record
     IMPORTING
       record_new    = wa_result-record.

     ADD wa_result-deb_cre_dc TO l_total_dc.
     ADD wa_result-deb_cre_lc TO l_total_lc.

* Controla que el total de la refinanciación no exceda la realizar el split
     AT END OF item_num.

       IF l_total_lc GT l_deb_cre_lc.
wa_result-/bic/zrc_dc_dc = wa_result-/bic/zrc_dc_dc - ( l_total_dc - l_deb_cre_dc ).
wa_result-/bic/zrc_dc_lc = wa_result-/bic/zrc_dc_lc - ( l_total_lc - l_deb_cre_lc ).
         wa_result-deb_cre_dc = wa_result-/bic/zrc_dc_dc.
         wa_result-deb_cre_lc = wa_result-/bic/zrc_dc_lc.

       ELSEIF l_total_lc LT l_deb_cre_lc.
wa_result-/bic/zrc_dc_dc = wa_result-/bic/zrc_dc_dc + ( l_deb_cre_dc - l_total_dc ).
wa_result-/bic/zrc_dc_lc = wa_result-/bic/zrc_dc_lc + ( l_deb_cre_lc - l_total_lc ).
         wa_result-deb_cre_dc = wa_result-/bic/zrc_dc_dc.
         wa_result-deb_cre_lc = wa_result-/bic/zrc_dc_lc.

       ENDIF.

       CLEAR: l_total_lc, l_total_dc.
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
