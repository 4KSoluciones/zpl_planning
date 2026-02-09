*&---------------------------------------------------------------------*
*&  Include  zpl_if_01_end_v1 - PxQ
*&---------------------------------------------------------------------*
*
*TYPES: BEGIN OF ty_wa_clave,
*         comp_code  TYPE /bi0/oicomp_code,
*         co_area    TYPE /bi0/oico_area,
*         gl_account TYPE /bi0/oigl_account,
*         profit_ctr TYPE /bi0/oiprofit_ctr,
*         coorder    TYPE /bi0/oicoorder,
*         username   type /bi0/oiusername,
*       END OF ty_wa_clave,
*       ty_lt_clave TYPE SORTED TABLE OF ty_wa_clave WITH NON-UNIQUE KEY
*              comp_code co_area gl_account profit_ctr coorder.

*


DATA: wa_result TYPE _ty_s_tg_1,
      lt_aux    TYPE  _ty_t_tg_1.





TRY.
    CHECK result_package[] IS NOT INITIAL.

    o_utiles =  NEW zpl_utiles( ).

    DATA(lt_claves) = CORRESPONDING o_utiles->ty_lt_claves( result_package ).
    DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder.
*-----------------------------------------------------------------------------



    IF sy-batch IS INITIAL.
      BREAK-POINT.
    ENDIF.




    SELECT *
    INTO TABLE @DATA(lt_montos_re)
    FROM zpl_gastos_real AS a
    FOR ALL ENTRIES IN @lt_claves
    WHERE a~comp_code = @lt_claves-comp_code
    AND   a~co_area   = @lt_claves-co_area
    AND  a~fiscper IN @o_utiles->gr_fiscper_pxq
    AND   a~gl_account = @lt_claves-gl_account
    AND   a~costcenter = @lt_claves-costcenter
    AND   a~coorder    = @lt_claves-coorder.

    IF sy-subrc EQ 0.
      SORT lt_montos_re BY comp_code fiscper gl_account costcenter coorder.
    ENDIF.


    SELECT *
      INTO TABLE @DATA(lt_montos_pre)
      FROM zpl_gastos_pre AS a
      FOR ALL ENTRIES IN @lt_claves
      WHERE a~comp_code = @lt_claves-comp_code
      AND   a~co_area   = @lt_claves-co_area
      AND   a~fiscper IN @o_utiles->gr_fiscper_pxq
      AND   a~gl_account = @lt_claves-gl_account
      AND   a~costcenter = @lt_claves-costcenter
      AND   a~coorder    = @lt_claves-coorder.

    IF sy-subrc EQ 0.
      SORT lt_montos_pre BY comp_code fiscper gl_account costcenter coorder.
    ENDIF.





******** Recorre las Claves del SOURCE_PACKAGE
    LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


*     Agrega los 12 periodos para cada Clave:
      LOOP AT o_utiles->gt_period_pxq ASSIGNING FIELD-SYMBOL(<fs_perio>).

        CLEAR wa_result.


        IF <fs_perio>-tipo EQ 'R'.
          READ TABLE  lt_montos_re ASSIGNING FIELD-SYMBOL(<fs_result>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                           fiscper    = <fs_perio>-period
                                                           gl_account = <fs_clave>-gl_account
                                                           costcenter = <fs_clave>-costcenter
                                                           coorder    = <fs_clave>-coorder
                                                       BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING <fs_result> TO wa_result.
            wa_result-/bic/zpl_gasto = <fs_result>-gastos * 100.
            wa_result-/bic/zquantity = <fs_result>-cantidad.

          ENDIF.

        ELSE.

          READ TABLE  lt_montos_pre ASSIGNING <fs_result>  WITH KEY comp_code  = <fs_clave>-comp_code
                                                           fiscper    = <fs_perio>-period
                                                           gl_account = <fs_clave>-gl_account
                                                           costcenter = <fs_clave>-costcenter
                                                           coorder    = <fs_clave>-coorder
                                                             BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING <fs_result> TO wa_result.
            wa_result-/bic/zpl_gasto = <fs_result>-gastos.
            wa_result-/bic/zquantity = <fs_result>-cantidad.

          ENDIF.
        ENDIF.

        IF sy-subrc EQ 0.

          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-unit    = 'UN'.
          wa_result-currency = 'PYG'.
          wa_result-curtype = '10'.
          wa_result-/bic/zpl_pgral = 1.
          wa_result-/bic/zquantity = 1.

          wa_result-version = o_utiles->g_version.


          IF wa_result-/bic/zquantity IS NOT INITIAL.
            wa_result-/bic/zpl_upric = wa_result-/bic/zpl_gasto / wa_result-/bic/zquantity.
            wa_result-/bic/zpl_qinpu = wa_result-/bic/zquantity.
            IF wa_result-/bic/zpl_qinpu IS INITIAL.
              wa_result-/bic/zpl_qinpu = 1.
            ENDIF.
          ELSEIF wa_result-/bic/zpl_gasto IS NOT INITIAL.
            wa_result-/bic/zpl_upric = wa_result-/bic/zpl_gasto.
            wa_result-/bic/zpl_qinpu = 1.
          ENDIF.

        ELSE.
*           Inicializa wa_result:
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-unit    = 'UN'.
          wa_result-currency = 'PYG'.
          wa_result-curtype = '10'.
          wa_result-/bic/zpl_upric = 0.
          wa_result-/bic/zpl_qinpu = 1.
          wa_result-/bic/zpl_pgral = 1.
          wa_result-version = o_utiles->g_version.


        ENDIF.


        CALL METHOD o_utiles->get_plan_version( IMPORTING o_plan_version = wa_result-/bic/ztabla ).


        CALL METHOD o_utiles->get_resp(
          EXPORTING
            i_soc    = wa_result-comp_code
            i_soco   = wa_result-co_area
            i_cuenta = wa_result-gl_account
            i_cebe   = wa_result-costcenter
            i_orden  = wa_result-coorder
          IMPORTING
            o_user   = wa_result-username
                       ).


        CALL METHOD o_utiles->get_plan_period
          EXPORTING
            i_tipo   = 'PXQ'
            i_period = <fs_perio>-period "Periodo de input/precarga
          IMPORTING
            o_plan   = wa_result-fiscper. "Periodo Plan


        wa_result-fiscyear = wa_result-fiscper(4).



        APPEND wa_result TO lt_aux.
        CLEAR wa_result.

      ENDLOOP." Periodos
    ENDLOOP." Claves Source Package




*-------------------------------------------
    result_package[] = lt_aux[].

  CATCH  cx_root INTO DATA(l_excepcion).

ENDTRY.
