*&---------------------------------------------------------------------*
*&  Include           ZPL_IF_O01_END
*&---------------------------------------------------------------------*

DATA: l_coef TYPE p DECIMALS 4.


TRY.
    CHECK result_package[] IS NOT INITIAL.



    IF sy-batch IS INITIAL.
      BREAK-POINT.
    ENDIF.

    SORT result_package BY comp_code fiscper gl_account profit_ctr coorder.


*1) Selecciona Claves para asignar el usuario
    SELECT
    a~comp_code,
    a~fiscper,
    a~gl_account,
    a~costcenter,
    a~coorder,
    a~username,
    a~CO_AREA,
    a~CHRT_ACCTS,
    a~FISCVARNT


    INTO TABLE @DATA(lt_claves)
    FROM /bic/azpl_a037 AS a
    FOR ALL ENTRIES IN @result_package
    WHERE a~comp_code EQ @result_package-comp_code
    AND   a~gl_account EQ @result_package-gl_account
    AND   a~costcenter EQ @result_package-profit_ctr
    AND   a~coorder    EQ @result_package-coorder.

    IF sy-subrc EQ 0.
      SORT lt_claves BY comp_code fiscper gl_account costcenter coorder.

    ENDIF.


    DATA: i         TYPE n LENGTH 3,
          l_fiscper TYPE /bi0/oifiscper,
          wa_result TYPE _ty_s_tg_1,
          lt_aux    TYPE  _ty_t_tg_1.



    CLEAR:  l_fiscper, wa_result.




******** Recorre las Claves dadas de Alta para el usuario:
    LOOP AT lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


      CLEAR l_fiscper .
      CONCATENATE '2023' <fs_clave>-fiscper+4(3) INTO l_fiscper.




      READ TABLE  result_package ASSIGNING <result_fields> WITH KEY comp_code  = <fs_clave>-comp_code
                                                               fiscper    = l_fiscper
                                                               gl_account = <fs_clave>-gl_account
                                                               profit_ctr = <fs_clave>-costcenter
                                                               coorder    = <fs_clave>-coorder
                                                           BINARY SEARCH.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <result_fields> TO wa_result.
      ELSE.
        CLEAR l_fiscper .
        CONCATENATE '2022' <fs_clave>-fiscper+4(3) INTO l_fiscper.
        READ TABLE  result_package ASSIGNING <result_fields> WITH KEY comp_code  = <fs_clave>-comp_code
                                                                 fiscper    = l_fiscper
                                                                 gl_account = <fs_clave>-gl_account
                                                                 profit_ctr = <fs_clave>-costcenter
                                                                 coorder    = <fs_clave>-coorder
                                                                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING <result_fields> TO wa_result.
        ELSE.
          MOVE-CORRESPONDING <fs_clave> TO wa_result.

        ENDIF.
      ENDIF.

      IF wa_result-/bic/zquantity IS NOT INITIAL.
        wa_result-/bic/zpl_upric = wa_result-/bic/zpl_gasto / wa_result-/bic/zquantity.
        wa_result-/bic/zpl_qinpu = wa_result-/bic/zquantity.
      ELSEif wa_result-/bic/zpl_gasto is not INITIAL.
        wa_result-/bic/zpl_upric = wa_result-/bic/zpl_gasto.
        wa_result-/bic/zpl_qinpu = 1.
      ENDIF.

      wa_result-username = <fs_clave>-username.
      wa_result-fiscper = <fs_clave>-fiscper.
      wa_result-fiscyear = <fs_clave>-fiscper(4).

      APPEND wa_result TO lt_aux.
      CLEAR wa_result.


    ENDLOOP.

    result_package[] = lt_aux[].

  CATCH  cx_root INTO DATA(l_excepcion).

ENDTRY.
