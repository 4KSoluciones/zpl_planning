*&---------------------------------------------------------------------*
*&  Include           ZPL_IF_I01_R_END
*&---------------------------------------------------------------------*
DATA: lo_utiles     TYPE REF TO zpl_utiles,
*      lt_gastos_filter TYPE SORTED TABLE OF zpl_gastorealsum
*      WITH NON-UNIQUE KEY co_area comp_code gl_account costcenter coorder,
      wa_result     TYPE  _ty_s_tg_1,
      lt_result_aux TYPE  _ty_t_tg_1.

IF lo_utiles IS NOT BOUND.
  lo_utiles = NEW zpl_utiles(  ).
ENDIF.
*

IF sy-batch IS INITIAL.
  BREAK-POINT.
ENDIF.


SELECT a~co_area, a~comp_code, a~gl_account, a~costcenter, a~coorder, a~gastos
  INTO TABLE @DATA(lt_gastos_filter)
  FROM zpl_gastos_real_sum AS a
  FOR ALL ENTRIES IN @result_package
  WHERE a~co_area EQ @result_package-co_area
  AND   a~comp_code EQ @result_package-comp_code
*  AND   a~fiscyear  IN @lo_utiles->gr_fiscyear
  AND   a~gl_account EQ @result_package-gl_account
  AND   a~costcenter EQ @result_package-costcenter
  AND   a~coorder    EQ @result_package-coorder.

IF sy-subrc EQ 0.
  SORT lt_gastos_filter BY co_area comp_code gl_account costcenter coorder.

ENDIF.



*  DATA(lt_aux) = FILTER #( result_package EXCEPT IN lt_gastos_filter
*                          WHERE co_area = co_area
*                          AND comp_code = comp_code
*                          AND gl_account = gl_account
*                          AND costcenter = costcenter
*                          AND coorder    = coorder ).




LOOP AT result_package ASSIGNING FIELD-SYMBOL(<fs_aux>).
  CLEAR wa_result.

  READ TABLE lt_gastos_filter ASSIGNING FIELD-SYMBOL(<fs_gastos>) WITH KEY co_area = <fs_aux>-co_area
                                       comp_code = <fs_aux>-comp_code
                                       gl_account = <fs_aux>-gl_account
                                       costcenter = <fs_aux>-costcenter
                                       coorder    = <fs_aux>-coorder
                                       BINARY SEARCH
                                          .

  "Regla 1: Si NO existe la combinación del Gasto Real en el período de plan,EXCLUIR.
  IF sy-subrc NE 0.
    CONTINUE.

  ELSE.
    "Regla 2: Si existe la combinación, pero el TOTAL es Cero en el periodo de plan, EXCLUIR
    IF <fs_gastos>-gastos EQ 0.
        continue.
    ELSE.
      CALL METHOD lo_utiles->get_resp
        EXPORTING
          i_soc    = <fs_aux>-comp_code
          i_soco   = <fs_aux>-co_area
          i_cuenta = <fs_aux>-gl_account
          i_cebe   = <fs_aux>-costcenter
          i_orden  = <fs_aux>-coorder
        IMPORTING
          o_user   = <fs_aux>-username.

      MOVE-CORRESPONDING <fs_aux> TO wa_result.
*
      LOOP AT lo_utiles->gt_period_pxq ASSIGNING FIELD-SYMBOL(<fs_periodo>).
        wa_result-fiscper = <fs_periodo>-plan.

        APPEND wa_result TO lt_result_aux.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDLOOP.

result_package[] = lt_result_aux[].
