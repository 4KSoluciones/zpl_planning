*&---------------------------------------------------------------------*
*&  Include           ZPL_IF_O03_END - Variable Cantidad
*&---------------------------------------------------------------------*
DATA: l_coef TYPE p DECIMALS 4.


TRY.
    CHECK result_package[] IS NOT INITIAL.



    IF sy-batch IS INITIAL.
      BREAK-POINT.
    ENDIF.

    SORT result_package BY fiscper matl_group sales_off /bic/zmar.
*1) Selecciona los Reales para calcular precio unitario
    SELECT
        a~comp_code,
        a~fiscper,
        a~linea,
        a~sucursal,
        a~marca,
        a~gastostotal,
        a~ventastotal,
        a~canttotal
     INTO TABLE @DATA(lt_data)
     FROM
      zpl_cebes_real( i_datum = @sy-datum ) AS a
      FOR ALL ENTRIES IN @result_package
    WHERE a~fiscyear EQ @result_package-fiscyear
    AND   a~linea EQ @result_package-matl_group
    AND   a~sucursal EQ @result_package-sales_off
    AND   a~marca    EQ @result_package-/bic/zmar
    AND   a~tipo EQ 'venta'.


    IF sy-subrc EQ 0.
      SORT lt_data BY comp_code fiscper linea sucursal marca.
    ENDIF.

*2) Selecciona la venta (Presupuesto) para calcular el Precio Unitario
    SELECT
      a~comp_code,
      a~fiscper,
      a~linea,
      a~sucursal,
      a~marca,
      a~gastostotal,
      a~ventastotal,
      a~canttotal
   INTO TABLE @DATA(lt_data_pres)
   FROM
    zpl_cebes_pres_u( i_datum = @sy-datum ) AS a
    FOR ALL ENTRIES IN @result_package
    WHERE a~fiscyear EQ @result_package-fiscyear
  AND   a~linea EQ @result_package-matl_group
  AND   a~sucursal EQ @result_package-sales_off
  AND   a~marca    EQ @result_package-/bic/zmar
  AND   a~tipo EQ 'venta'.
    IF sy-subrc EQ 0.
      SORT lt_data_pres BY comp_code fiscper linea sucursal marca.
    ENDIF.


*3) Selecciona Claves para asignar el usuario
    SELECT
    a~comp_code,
    a~fiscper,
    a~gl_account,
    a~costcenter,
    a~coorder,
    a~username
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


    DELETE ADJACENT DUPLICATES FROM result_package COMPARING comp_code gl_account profit_ctr coorder.

    DATA: i TYPE n LENGTH 3,
          l_fiscper type /bi0/oifiscper,
          wa_result type _ty_s_tg_1,
          lt_aux type  _ty_t_tg_1.

    LOOP AT  result_package ASSIGNING <result_fields>.
      i = '000'.
      DO 12 TIMES.

        ADD 1 TO i.
        clear:  l_fiscper, wa_result.

        MOVE-CORRESPONDING <result_fields> to wa_result.
        CONCATENATE <result_fields>-fiscyear i INTO l_fiscper.


******** REALES:
        READ TABLE lt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WITH KEY  comp_code = wa_result-comp_code
                                                                       fiscper    = l_fiscper
                                                                       linea = wa_result-matl_group
                                                                       sucursal  = wa_result-sales_off
                                                                       marca  = wa_result-/bic/zmar BINARY SEARCH.

        IF sy-subrc EQ 0 AND <fs_data>-canttotal <> 0.

          wa_result-/bic/zpl_venta = <fs_data>-ventastotal.
          wa_result-/bic/zpl_vtaq =  <fs_data>-canttotal.
          wa_result-/bic/zpl_upric = <fs_data>-ventastotal / <fs_data>-canttotal.




******** PRESUPUESTO:  DEBERIA SER FISCYEAR + 1 (2024) PERO PARA PRUEBAS SE MANTIENE EL 2023
          READ TABLE lt_data_pres ASSIGNING FIELD-SYMBOL(<fs_data_pres>) WITH KEY
                                                                         comp_code = wa_result-comp_code
                                                                         fiscper   = l_fiscper
                                                                         linea     = wa_result-matl_group
                                                                         sucursal  = wa_result-sales_off
                                                                         marca     = wa_result-/bic/zmar BINARY SEARCH.
          IF sy-subrc EQ 0.
            wa_result-/bic/zpl_ventp =  <fs_data_pres>-ventastotal.
            wa_result-/bic/zpl_vtaqp = <fs_data_pres>-canttotal.
          ENDIF.
        ENDIF.


        DATA(l_dif) = '2024' - wa_result-fiscyear.
        ADD l_dif TO wa_result-fiscyear.
        CONCATENATE wa_result-fiscyear l_fiscper+4(3) INTO wa_result-fiscper.


        READ TABLE lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>) WITH KEY
                                                                    comp_code  = wa_result-comp_code
                                                                    fiscper    = wa_result-fiscper
                                                                    gl_account = wa_result-gl_account
                                                                    costcenter = wa_result-profit_ctr
                                                                    coorder    = wa_result-coorder
                                                                    BINARY SEARCH.

        IF sy-subrc EQ 0.
          wa_result-username = <fs_clave>-username.
        ENDIF.

          append wa_result to lt_aux.
      ENDDO.
    ENDLOOP.

    result_package[] = lt_aux[].

  CATCH  cx_root INTO DATA(l_excepcion).

ENDTRY.
