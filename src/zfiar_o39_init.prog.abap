*&---------------------------------------------------------------------*
*&  Include           ZFIAR_O39_INIT - Cheques Emisor (S)
*&---------------------------------------------------------------------*
data: wa_source type _ty_s_SC_1,
      lt_aux    type _ty_t_SC_1.



if sy-batch is iNITIAL.
  BREAK-POINT.

endif.

    IF gt_tvarv[] IS INITIAL.
      SELECT *
        INTO TABLE gt_tvarv
        FROM ztvarv_be
        where name eq 'S_REF'.

      IF sy-subrc EQ 0.
        SORT gt_tvarv BY low high.
      ENDIF.
    ENDIF.


    LOOP AT SOURCE_PACKAGE ASSIGNING <source_fields>.


      READ TABLE gt_tvarv WITH KEY  low = <source_fields>-post_key
                                    high = <source_fields>-ac_doc_typ
                                    TRANSPORTING NO FIELDS.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <source_fields> to wa_source.
        append wa_source to lt_aux.

      ENDIF.


    ENDLOOP.


SOURCE_PACKAGE[] = lt_aux[].
