*&---------------------------------------------------------------------*
*&  Include  zfiar_o40_init - Receptor (H)
*&---------------------------------------------------------------------*


DATA: wa_source TYPE _ty_s_sc_1,
      lt_aux    TYPE _ty_t_sc_1.



IF sy-batch IS INITIAL.
  BREAK-POINT.

ENDIF.

IF gt_tvarv[] IS INITIAL.
  SELECT *
    INTO TABLE gt_tvarv
    FROM ztvarv_be
    WHERE name EQ 'H_REF'.

  IF sy-subrc EQ 0.
    SORT gt_tvarv BY low high.
  ENDIF.
ENDIF.


LOOP AT source_package ASSIGNING <source_fields>.


  READ TABLE gt_tvarv WITH KEY  low = <source_fields>-post_key
                                high = <source_fields>-ac_doc_typ
                                TRANSPORTING NO FIELDS.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING <source_fields> TO wa_source.
    APPEND wa_source TO lt_aux.

  ENDIF.


ENDLOOP.


source_package[] = lt_aux[].
