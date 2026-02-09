*&---------------------------------------------------------------------*
*&  Include           ZPL_IF_O07_END
*&---------------------------------------------------------------------*
DATA: lo_utiles      TYPE REF TO zpl_utiles,
      lt_claves      TYPE zpl_utiles=>ty_lt_claves_aux,
      lt_result      TYPE zpl_utiles=>ty_lt_result_varq,
      lt_result_2000 TYPE zpl_utiles=>ty_lt_result_varq.



IF sy-batch IS INITIAL.

  BREAK-POINT.
ENDIF.


IF lo_utiles IS NOT BOUND.
  lo_utiles = NEW zpl_utiles( ).

ENDIF.

SORT result_package BY comp_code co_area fiscper version coorder costcenter  gl_account username.


lt_claves = CORRESPONDING #( result_package ).




lo_utiles->get_varq_gasto_actual(
  EXPORTING
    i_claves = lt_claves
  IMPORTING
    e_result = lt_result
).



SORT lt_result BY comp_code co_area fiscper version coorder costcenter  gl_account username.

LOOP AT result_package ASSIGNING <result_fields>.



  READ TABLE  lt_result ASSIGNING  FIELD-SYMBOL(<fs_result>) WITH KEY comp_code = <result_fields>-comp_code
                                                                fiscper   = <result_fields>-fiscper
                                                                coorder   = <result_fields>-coorder
                                                                costcenter = <result_fields>-costcenter
                                                                gl_account = <result_fields>-gl_account
                                                                username   = <result_fields>-username
                                                                BINARY SEARCH.
  IF sy-subrc EQ 0.

    <result_fields>-/bic/zpl_gasto = <fs_result>-/bic/zpl_gasto.
    <result_fields>-/bic/zpl_gastq = <fs_result>-/bic/zpl_gastq.
  else.
    clear: <result_fields>-/bic/zpl_gasto, <result_fields>-/bic/zpl_gastq.
  ENDIF.



ENDLOOP.
