*&---------------------------------------------------------------------*
*&  Include  zpl_if_o03_end_v1 -  Variable Cantidad
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_wa_aux,
         comp_code  TYPE /bi0/oicomp_code,
         co_area    TYPE /bi0/oico_area,
         gl_account TYPE /bi0/oigl_account,
         profit_ctr TYPE /bi0/oiprofit_ctr,
         coorder    TYPE /bi0/oicoorder,
       END OF ty_wa_aux,
       ty_lt_aux TYPE SORTED TABLE OF ty_wa_aux WITH NON-UNIQUE KEY
              comp_code gl_account profit_ctr coorder.


DATA:
  wa_result TYPE _ty_s_tg_1,
  lt_aux    TYPE  _ty_t_tg_1.

DATA: o_utiles TYPE REF TO zpl_utiles.
DATA: l_coef TYPE p DECIMALS 4.

DATA: lr_fiscper_re  TYPE RANGE OF /bi0/oifiscper,
      lr_fiscper_pre TYPE RANGE OF /bi0/oifiscper,
      lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
      wr_fiscper     LIKE LINE OF lr_fiscper_re,
      lt_glaccount   TYPE RANGE OF /bi0/oigl_account.

TRY.



    CHECK result_package[] IS NOT INITIAL.
    o_utiles =  NEW zpl_utiles( ).


    DATA(lt_claves) = CORRESPONDING o_utiles->ty_lt_claves( result_package MAPPING costcenter = profit_ctr ).
    DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder.
*-----------------------------------------------------------------------------





    LOOP AT o_utiles->gt_period_var ASSIGNING FIELD-SYMBOL(<fs_period_var>).


      wr_fiscper-low = <fs_period_var>-actual.
      wr_fiscper-option = 'EQ'.
      wr_fiscper-sign   = 'I'.

      IF <fs_period_var>-tipo EQ 'R'.
        APPEND wr_fiscper TO lr_fiscper_re.
      ELSE.
        APPEND wr_fiscper TO lr_fiscper_pre.
      ENDIF.
    ENDLOOP.

    IF sy-batch IS INITIAL.
      BREAK-POINT.
    ENDIF.



* *1) GASTOS a nivel CUENTA CEBE ORDEN - Tomar Cantidades
    SELECT *
   INTO TABLE @DATA(lt_gastos_re)
   FROM zpl_gastos_varq AS a
   FOR ALL ENTRIES IN @lt_claves
   WHERE a~fiscper IN @lr_fiscper_re
   AND   a~gl_account = @lt_claves-gl_account
   AND   a~profit_ctr = @lt_claves-costcenter
   AND   a~coorder    = @lt_claves-coorder.

    IF sy-subrc EQ 0.
      SORT lt_gastos_re BY comp_code fiscper gl_account profit_ctr coorder.
    ENDIF.

    SELECT *
    INTO TABLE @DATA(lt_gastos_pre)
    FROM zpl_gastos_pre_varq AS a
    FOR ALL ENTRIES IN @lt_claves
    WHERE a~fiscper IN @lr_fiscper_pre
    AND   a~gl_account = @lt_claves-gl_account
    AND   a~profit_ctr = @lt_claves-costcenter
    AND   a~coorder    = @lt_claves-coorder.

    IF sy-subrc EQ 0.
      SORT lt_gastos_pre BY comp_code fiscper gl_account profit_ctr coorder.
    ENDIF.

*
**2) VENTAS REALES Agrupadas por Linea, Of Vta y Marca
**2.1) Selecciona los Reales para calcular coeficiente de Participacion del Gasto sobre la Venta
*    SORT result_package BY fiscper matl_group sales_off /bic/zmar.
*
*    IF lr_fiscper[] IS NOT INITIAL.
**2.1.1)      Busca las ventas agrupadas por Sucursal, linea y marca
*      SELECT *
*      INTO TABLE @DATA(lt_ventas_re)
*      FROM
*       zpl_cebes_real( i_datum = @sy-datum ) AS a
*       FOR ALL ENTRIES IN @result_package
*     WHERE a~comp_code EQ @result_package-comp_code
*     AND   a~fiscper IN @lr_fiscper
*     AND   a~linea EQ @result_package-matl_group
*     AND   a~marca    EQ @result_package-/bic/zmar
*     AND   a~sucursal EQ @result_package-sales_off.
*
*      IF sy-subrc EQ 0.
*        SORT lt_ventas_re BY comp_code fiscper linea sucursal marca.
*      ENDIF.
*
**2.1.2)   Busca las ventas agrupadas por Sucursal y Línea
*      SELECT *
*     INTO TABLE @DATA(lt_ventas_suc_lin)
*     FROM
*      zpl_vta_suc_linea( i_datum = @sy-datum ) AS a
*      FOR ALL ENTRIES IN @result_package
*    WHERE a~comp_code EQ @result_package-comp_code
*    AND   a~fiscper IN @lr_fiscper
*    AND   a~linea EQ @result_package-matl_group
*    AND   a~sucursal EQ @result_package-sales_off
*    AND   a~ventastotal NE 0.
*
*      IF sy-subrc EQ 0.
*        SORT lt_ventas_suc_lin BY comp_code fiscper linea sucursal.
*      ENDIF.
*
**2.1.3)   Busca las ventas agrupadas por Sucursal
*      SELECT *
*              INTO TABLE @DATA(lt_ventas_suc_)
*              FROM
*               zpl_vta_suc_( i_datum = @sy-datum ) AS a
*               FOR ALL ENTRIES IN @result_package
*             WHERE a~comp_code EQ @result_package-comp_code
*             AND   a~fiscper IN @lr_fiscper
*              AND  a~sucursal EQ @result_package-sales_off
*              AND  a~ventastotal NE 0.
*      IF sy-subrc EQ 0.
*        SORT lt_ventas_suc_ BY comp_code fiscper sucursal.
*      ENDIF.
*
*    ENDIF.

    SORT result_package BY fiscper matl_group sales_off /bic/zmar.
    DELETE ADJACENT DUPLICATES FROM result_package COMPARING fiscper matl_group sales_off /bic/zmar.


***VENTAS PRESUPUESTADAS
*3.1) Selecciona la venta (Presupuesto) para calcular el Gasto Unitario
    CLEAR lr_fiscper_vta[].
    LOOP AT o_utiles->gt_period_var ASSIGNING <fs_period_var>.

      wr_fiscper-low = <fs_period_var>-plan.
      wr_fiscper-option = 'EQ'.
      wr_fiscper-sign   = 'I'.
      APPEND wr_fiscper TO lr_fiscper_vta.

    ENDLOOP.



    SELECT *
   INTO TABLE @DATA(lt_vtas_pres)
   FROM
    zpl_cebes_pres_u( i_datum = @sy-datum ) AS a
    FOR ALL ENTRIES IN @result_package
    WHERE a~fiscper IN @lr_fiscper_vta
  AND   a~linea EQ @result_package-matl_group
  AND   a~sucursal EQ @result_package-sales_off
  AND   a~marca    EQ @result_package-/bic/zmar
  AND   a~tipo EQ 'venta'.
    IF sy-subrc EQ 0.
      SORT lt_vtas_pres BY comp_code fiscper linea sucursal marca.
    ENDIF.

*3.2)   Busca las ventas agrupadas por Sucursal y Línea
    SELECT *
   INTO TABLE @DATA(lt_vtas_pres_suc_lin)
   FROM
    zpl_vtas_pres_suc_linea( i_datum = @sy-datum ) AS a
    FOR ALL ENTRIES IN @result_package
  WHERE a~comp_code EQ @result_package-comp_code
  AND   a~fiscper IN @lr_fiscper_vta
  AND   a~linea EQ @result_package-matl_group
  AND   a~sucursal EQ @result_package-sales_off
  AND   a~ventastotal NE 0.

    IF sy-subrc EQ 0.
      SORT lt_vtas_pres_suc_lin BY comp_code fiscper linea sucursal.
    ENDIF.

*2.3)   Busca las ventas agrupadas por Sucursal
    SELECT *
            INTO TABLE @DATA(lt_vtas_pres_suc_)
            FROM
             zpl_vtas_pres_suc_( i_datum = @sy-datum ) AS a
             FOR ALL ENTRIES IN @result_package
           WHERE a~comp_code EQ @result_package-comp_code
           AND   a~fiscper IN @lr_fiscper_vta
            AND  a~sucursal EQ @result_package-sales_off
            AND  a~ventastotal NE 0.
    IF sy-subrc EQ 0.
      SORT lt_vtas_pres_suc_ BY comp_code fiscper sucursal.
    ENDIF.







*3) Obtengo para cada CEBE: Linea, Of de Venta y Marca

    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

    SELECT b~costcenter,
    b~sales_off,
    b~matl_group,
    b~/bic/zmar
    INTO TABLE @DATA(lt_costcenter)
   FROM /bi0/mcostcenter  AS b
   FOR ALL ENTRIES IN @lt_claves
   WHERE co_area = @lt_claves-co_area
   AND costcenter = @lt_claves-costcenter
   AND  b~objvers EQ 'A'
   AND ( b~datefrom LE @sy-datum AND b~dateto GE @sy-datum ).
    IF sy-subrc EQ 0.
      SORT lt_costcenter BY costcenter ASCENDING.
      DELETE lt_costcenter WHERE sales_off EQ '' AND matl_group EQ '' AND /bic/zmar EQ ''.
    ENDIF.





*4) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
    READ TABLE lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>)  INDEX 1.
    IF sy-subrc EQ 0.

      SELECT 'I' AS sign, 'EQ' AS option, a~gl_account
      INTO TABLE @lt_glaccount
      FROM /bic/azpl_a292 AS a
      WHERE a~comp_code EQ @<fs_clave>-comp_code
      AND   a~/bic/zpl_dum01 EQ '1'.

    ENDIF.

**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************


    LOOP AT  lt_claves ASSIGNING <fs_clave>.

******* Agrupadores Sucursal, LInea Marca
      READ TABLE lt_costcenter ASSIGNING FIELD-SYMBOL(<fs_costcenter>) WITH KEY costcenter = <fs_clave>-costcenter.
      CHECK sy-subrc EQ 0.

      CLEAR: l_coef,  wa_result.

*     Inicializa wa_result:
      MOVE-CORRESPONDING <fs_clave> TO wa_result.
      MOVE-CORRESPONDING <fs_costcenter> TO wa_result.
      wa_result-profit_ctr = <fs_costcenter>-costcenter.
      wa_result-chrt_accts = 'SQS'.
      wa_result-fiscvarnt  = 'K4'.
      wa_result-/bic/ztabla = o_utiles->c_plan.
      wa_result-version = o_utiles->c_version.
      wa_result-unit    = 'UN'.
      wa_result-currency = 'PYG'.



*     Agrega los 12 periodos para cada Clave:
      LOOP AT o_utiles->gt_period_varq ASSIGNING FIELD-SYMBOL(<fs_perio>).

        CLEAR: wa_result-/bic/zpl_qinpu, wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp,
                        wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp, wa_result-/bic/zpl_upric.
*----------------------------------------------------------------------------*
******** VENTA PRESUPUESTADA: Por Linea, Sucursal y Marca
        READ TABLE lt_vtas_pres ASSIGNING FIELD-SYMBOL(<fs_presup>) WITH KEY
                                                                       comp_code = <fs_clave>-comp_code
                                                                       fiscper   = <fs_perio>-plan
                                                                       linea     = <fs_costcenter>-matl_group
                                                                       sucursal  = <fs_costcenter>-sales_off
                                                                       marca     = <fs_costcenter>-/bic/zmar BINARY SEARCH.
        IF sy-subrc EQ 0.

          wa_result-/bic/zpl_ventp =  abs( <fs_presup>-ventastotal * 100 ).
          wa_result-/bic/zpl_vtaqp = abs( <fs_presup>-canttotal ).
*            wa_result-/bic/zpl_gastp =  abs( <fs_presup>-gastostotal ).Todavía no hay datos de presupuesto gastos
        ELSE.
*          Busca las ventas PRESUPUESTADAS agrupadas por Linea y Sucursal
          READ TABLE lt_vtas_pres_suc_lin ASSIGNING FIELD-SYMBOL(<fs_pres_suc_lin>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                       fiscper   = <fs_perio>-plan
                                                                       linea     = <fs_costcenter>-matl_group
                                                                       sucursal  = <fs_costcenter>-sales_off
                                                                       BINARY SEARCH.
          IF sy-subrc EQ 0.
            wa_result-/bic/zpl_ventp = abs( <fs_pres_suc_lin>-ventastotal * 100 ).
            wa_result-/bic/zpl_vtaqp  = abs( <fs_pres_suc_lin>-canttotal ).
          ELSE.
*            Busca las ventas agrupadas por sucursal
            READ TABLE lt_vtas_pres_suc_ ASSIGNING FIELD-SYMBOL(<fs_pres_suc_>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                         fiscper   = <fs_perio>-plan
                                                                         sucursal  = <fs_costcenter>-sales_off
                                                                         BINARY SEARCH.
            IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN o_utiles->gt_cuentas.
              wa_result-/bic/zpl_ventp = abs( <fs_pres_suc_>-ventastotal * 100 ).
              wa_result-/bic/zpl_vtaqp  = abs( <fs_pres_suc_>-canttotal ).
            ENDIF."Sucursal
          ENDIF."Linea, Sucursal
        ENDIF."Sucursal, Linea Marca

*--------------------------------------------------------------*
********VENTAS REALES:
*        READ TABLE lt_ventas_re ASSIGNING FIELD-SYMBOL(<fs_venta>) WITH KEY  comp_code = <fs_clave>-comp_code
*                                                                         fiscper   = <fs_perio>-period
*                                                                         linea     = <fs_profit>-matl_group
*                                                                         sucursal  = <fs_profit>-sales_off
*                                                                         marca     = <fs_profit>-/bic/zmar
*                                                                         BINARY SEARCH.
*
*        IF sy-subrc EQ 0.
*          wa_result-/bic/zpl_venta = abs( <fs_venta>-ventastotal * 100 ).
*          wa_result-/bic/zpl_vtaq  = abs( <fs_venta>-canttotal ).
*
*        ELSE.
**          Busca las ventas agrupadas por Linea y Sucursal
*          READ TABLE lt_ventas_suc_lin ASSIGNING FIELD-SYMBOL(<fs_suc_lin>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                       fiscper   = <fs_perio>-period
*                                                                       linea     = <fs_profit>-matl_group
*                                                                       sucursal  = <fs_profit>-sales_off
*                                                                       BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            wa_result-/bic/zpl_venta = abs( <fs_suc_lin>-ventastotal * 100 ).
*            wa_result-/bic/zpl_vtaq  = abs( <fs_venta>-canttotal ).
*          ELSE.
**            Busca las ventas agrupadas por sucursal
*            READ TABLE lt_ventas_suc_ ASSIGNING FIELD-SYMBOL(<fs_suc_>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                         fiscper   = <fs_perio>-period
*                                                                         sucursal  = <fs_profit>-sales_off
*                                                                         BINARY SEARCH.
*            IF sy-subrc EQ 0.
*              wa_result-/bic/zpl_venta = abs( <fs_suc_>-ventastotal * 100 ).
*              wa_result-/bic/zpl_vtaq  = abs( <fs_venta>-canttotal ).
*            ENDIF."Sucursal
*          ENDIF."Linea, Sucursal
*        ENDIF."Sucursal, Linea Marca
*




*---------------------------------------
****   GASTOS REALES:

        IF <fs_perio>-tipo EQ 'R'.
          READ TABLE  lt_gastos_re ASSIGNING FIELD-SYMBOL(<fs_gasto>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                           fiscper    = <fs_perio>-actual
                                                           gl_account = <fs_clave>-gl_account
                                                           profit_ctr = <fs_clave>-costcenter
                                                           coorder    = <fs_clave>-coorder
                                                       BINARY SEARCH.
        ELSE.

          READ TABLE  lt_gastos_pre ASSIGNING <fs_gasto>  WITH KEY comp_code  = <fs_clave>-comp_code
                                                           fiscper    = <fs_perio>-actual
                                                           gl_account = <fs_clave>-gl_account
                                                           profit_ctr = <fs_clave>-costcenter
                                                           coorder    = <fs_clave>-coorder
                                                       BINARY SEARCH.
        ENDIF.

        IF sy-subrc EQ 0.
          DATA(l_gasto)    = abs( <fs_gasto>-gastos * 100 ).
          DATA(l_gastoq)   = abs( <fs_gasto>-cantidad ).
        ENDIF.

*--------------------------------------

        IF wa_result-/bic/zpl_vtaqp <> 0.
          wa_result-/bic/zpl_upric = l_gasto / wa_result-/bic/zpl_vtaqp.
        ELSE.
          wa_result-/bic/zpl_upric  = 0.
        ENDIF.



        CALL METHOD o_utiles->get_resp(
          EXPORTING
            i_soc    = wa_result-comp_code
            i_soco   = wa_result-co_area
            i_cuenta = wa_result-gl_account
            i_cebe   = wa_result-profit_ctr
            i_orden  = wa_result-coorder
          IMPORTING
            o_user   = wa_result-username
                       ).


        CALL METHOD o_utiles->get_plan_period
          EXPORTING
            i_tipo   = 'VARQ'
            i_period = <fs_perio>-actual "Periodo de input/precarga
          IMPORTING
            o_plan   = wa_result-fiscper. "Periodo Plan


        wa_result-fiscyear = wa_result-fiscper(4).

        APPEND wa_result TO lt_aux.

      ENDLOOP." Periodos
    ENDLOOP." Claves Source Package

    result_package[] = lt_aux[].

  CATCH  cx_root INTO DATA(l_excepcion).

ENDTRY.
