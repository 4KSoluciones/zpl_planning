*&---------------------------------------------------------------------*
*&  Include  zpl_if_o02_end_soc2000
*&---------------------------------------------------------------------*

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

         wr_fiscper-low = <fs_period_var>-period.
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


* *1) GASTOS a nivel CUENTA CEBE ORDEN - Fuente CEBES (ZPCA_O01N)
*   1.1 Gasto Reales
       SELECT *
       INTO TABLE @DATA(lt_gastos_re)
       FROM zpl_gastos_var AS a
       FOR ALL ENTRIES IN @lt_claves
       WHERE a~comp_code EQ @lt_claves-comp_code
       AND   a~fiscper IN @lr_fiscper_re
       AND   a~gl_account = @lt_claves-gl_account
       AND   a~costcenter = @lt_claves-costcenter
       AND   a~coorder    = @lt_claves-coorder.

       IF sy-subrc EQ 0.
         SORT lt_gastos_re BY comp_code fiscper gl_account costcenter coorder.
       ENDIF.

*   1.2 Gasto Presupuesto 23V0
       SELECT *
       INTO TABLE @DATA(lt_gastos_pre)
       FROM zpl_gastos_pre_var AS a
       FOR ALL ENTRIES IN @lt_claves
       WHERE a~comp_code EQ @lt_claves-comp_code
       AND   a~fiscper IN @lr_fiscper_pre
       AND   a~gl_account = @lt_claves-gl_account
       AND   a~costcenter = @lt_claves-costcenter
       AND   a~coorder    = @lt_claves-coorder.

       IF sy-subrc EQ 0.
         SORT lt_gastos_pre BY comp_code fiscper gl_account costcenter coorder.
       ENDIF.


*----------------------------------------------
*2) VENTAS REALES Agrupadas por Sucursal, Canal, Linea,  Marca - Fuente Reales CEBE (ZPCA_O01N)
*2.1) Selecciona los Reales para calcular coeficiente de Participacion del Gasto sobre la Venta
       SORT result_package BY fiscper sales_off distr_chan matl_group /bic/zmar.


       SELECT * "Explicito el * para que la modificacion solo deba ser a nivel CDS
        INTO TABLE @DATA(lt_vta_real_s1)
        FROM
         zpl_vta_real_s2_01_sum( i_datum = @sy-datum ) AS a
         FOR ALL ENTRIES IN @result_package
       WHERE a~comp_code EQ @result_package-comp_code
       AND   ( a~fiscper IN @lr_fiscper_re OR
               a~fiscper IN @lr_fiscper_pre )
       AND   a~sucursal EQ @result_package-sales_off
       AND   a~canal    EQ @result_package-distr_chan
       AND   a~linea EQ @result_package-matl_group
       AND   a~marca    EQ @result_package-/bic/zmar.

       IF sy-subrc EQ 0.
         SORT lt_vta_real_s1 BY comp_code fiscper sucursal canal linea  marca.
       ENDIF.


*2.1.2)   Busca las ventas agrupadas por Sucursal, Canal y Línea
       SELECT *
       INTO TABLE @DATA(lt_vta_real_s2)
       FROM
        zpl_vta_real_s2_02_sum( i_datum = @sy-datum ) AS a
        FOR ALL ENTRIES IN @result_package
      WHERE a~comp_code EQ @result_package-comp_code
      AND   ( a~fiscper IN @lr_fiscper_re OR
             a~fiscper IN @lr_fiscper_pre )
      AND   a~sucursal EQ @result_package-sales_off
       AND   a~canal    EQ @result_package-distr_chan
       AND   a~linea EQ @result_package-matl_group
      AND   a~ventastotal NE 0.

       IF sy-subrc EQ 0.
         SORT lt_vta_real_s2 BY comp_code fiscper sucursal canal linea.
       ENDIF.

*2.1.3)   Busca las ventas agrupadas por Sucursal, Canal
       SELECT *
         INTO TABLE @DATA(lt_vta_real_s3)
         FROM
          zpl_vta_real_s2_03_sum( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @result_package
        WHERE a~comp_code EQ @result_package-comp_code
        AND   ( a~fiscper IN @lr_fiscper_re OR
             a~fiscper IN @lr_fiscper_pre )
         AND  a~sucursal EQ @result_package-sales_off
         AND   a~canal    EQ @result_package-distr_chan
         AND  a~ventastotal NE 0.
       IF sy-subrc EQ 0.
         SORT lt_vta_real_s3 BY comp_code fiscper sucursal canal.
       ENDIF.




*2.1.4)   Busca las ventas agrupadas por Sucursal
       SELECT *
       INTO TABLE @DATA(lt_vta_real_s4)
       FROM
        zpl_vta_real_s2_04_sum( i_datum = @sy-datum ) AS a
        FOR ALL ENTRIES IN @result_package
      WHERE a~comp_code EQ @result_package-comp_code
      AND   ( a~fiscper IN @lr_fiscper_re OR
           a~fiscper IN @lr_fiscper_pre )
       AND  a~sucursal EQ @result_package-sales_off
       AND  a~ventastotal NE 0.
       IF sy-subrc EQ 0.
         SORT lt_vta_real_s4 BY comp_code fiscper sucursal.
       ENDIF.



*----------------------------------------------
***VENTAS PRESUPUESTADAS (fuente COPA - 23V0 por ejemplo) - ZCOP_A01
*2) Selecciona la venta PRESUPUESTADA para calcular el Gasto Presupuestado = Vta Pres * Coef
       CLEAR lr_fiscper_vta[].
       LOOP AT o_utiles->gt_period_var ASSIGNING <fs_period_var>.

         wr_fiscper-low = <fs_period_var>-plan.
         wr_fiscper-option = 'EQ'.
         wr_fiscper-sign   = 'I'.
         APPEND wr_fiscper TO lr_fiscper_vta.

       ENDLOOP.

    if not lr_fiscper_pre[] is initial.
        append lines of lr_fiscper_pre to lr_fiscper_vta.
    endif.


*2.1)Clave completa: Sucursal, Canal, Linea, Marca
       SELECT *
      INTO TABLE @DATA(lt_vta_pres_s1)
      FROM
       zpl_vta_pres_s2_01_sum( i_datum = @sy-datum ) AS a
       FOR ALL ENTRIES IN @result_package
     WHERE a~comp_code EQ @result_package-comp_code
      AND  a~fiscper IN @lr_fiscper_vta
     AND   a~sucursal EQ @result_package-sales_off
     AND   a~canal    EQ @result_package-distr_chan
     AND   a~linea EQ @result_package-matl_group
     AND   a~marca    EQ @result_package-/bic/zmar
     AND   a~tipo EQ 'venta'.
       IF sy-subrc EQ 0.
         SORT lt_vta_pres_s1 BY comp_code fiscper sucursal canal linea marca.
       ENDIF.


*2,2) Sucursal, Canal, Linea:S2
       SELECT *
         INTO TABLE @DATA(lt_vta_pres_s2)
         FROM
          zpl_vta_pres_s2_02_sum( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @result_package
        WHERE a~comp_code EQ @result_package-comp_code
        AND  ( a~fiscper IN @lr_fiscper_vta OR
             a~fiscper IN @lr_fiscper_pre )
        AND   a~sucursal EQ @result_package-sales_off
        AND   a~canal    EQ @result_package-distr_chan
        AND   a~linea EQ @result_package-matl_group.
       IF sy-subrc EQ 0.
         SORT lt_vta_pres_s2 BY comp_code fiscper sucursal canal linea.
       ENDIF.


*2.3) Sucursal, Canal: S3
       SELECT *
         INTO TABLE @DATA(lt_vta_pres_s3)
         FROM
          zpl_vta_pres_s2_03_sum( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @result_package
        WHERE  a~comp_code EQ @result_package-comp_code
        AND   a~fiscper IN @lr_fiscper_vta
        AND   a~sucursal EQ @result_package-sales_off
        AND   a~canal    EQ @result_package-distr_chan.
       IF sy-subrc EQ 0.
         SORT lt_vta_pres_s3 BY comp_code fiscper sucursal canal.
       ENDIF.

*2.4) Sucursal: S4
       SELECT *
         INTO TABLE @DATA(lt_vta_pres_s4)
         FROM
          zpl_vta_pres_s2_04_sum( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @result_package
        WHERE  a~comp_code EQ @result_package-comp_code
        AND  ( a~fiscper IN @lr_fiscper_vta OR
             a~fiscper IN @lr_fiscper_pre )
        AND   a~sucursal EQ @result_package-sales_off.
       IF sy-subrc EQ 0.
         SORT lt_vta_pres_s4 BY comp_code fiscper sucursal.
       ENDIF.





*3) Obtengo para cada CEBE: Linea, Of de Venta y Marca

*    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
*    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

       SELECT b~costcenter,
       b~sales_off,
       b~distr_chan,
       b~matl_group,
       b~/bic/zmar
       INTO TABLE @DATA(lt_costcenter)
      FROM /bi0/mcostcenter AS b
      FOR ALL ENTRIES IN @lt_claves
      WHERE co_area = @lt_claves-co_area
      AND costcenter = @lt_claves-costcenter
      AND  b~objvers EQ 'A'
      AND ( b~datefrom LE @sy-datum AND b~dateto GE @sy-datum ).
       IF sy-subrc EQ 0.
         SORT lt_costcenter BY costcenter ASCENDING.
         DELETE lt_costcenter WHERE sales_off EQ '' AND matl_group EQ '' AND /bic/zmar EQ '' AND distr_chan EQ ''.
       ENDIF.



*4) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
       READ TABLE lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>)  INDEX 1.

       SELECT 'I' AS sign, 'EQ' AS option, a~gl_account
       INTO TABLE @lt_glaccount
       FROM /bic/azpl_a292 AS a
       WHERE a~comp_code EQ @<fs_clave>-comp_code
       AND   a~/bic/zpl_dum01 EQ '1'.

**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************
       LOOP AT  lt_claves ASSIGNING <fs_clave>.


******* Agrupadores Sucursal, LInea Marca
         READ TABLE lt_costcenter ASSIGNING FIELD-SYMBOL(<fs_cost>) WITH KEY costcenter = <fs_clave>-costcenter.
         CHECK sy-subrc EQ 0.

         CLEAR: l_coef,  wa_result.

*     Inicializa wa_result:
         MOVE-CORRESPONDING <fs_clave> TO wa_result.
         MOVE-CORRESPONDING <fs_cost> TO wa_result.
         wa_result-profit_ctr = <fs_cost>-costcenter.
         wa_result-chrt_accts = 'SQS'.
         wa_result-fiscvarnt  = 'K4'.
         CALL METHOD o_utiles->get_plan_version( IMPORTING o_plan_version = wa_result-/bic/ztabla ).
         wa_result-version = '000'.
         wa_result-unit    = 'UN'.
         wa_result-currency = 'PYG'.
         wa_result-version = o_utiles->g_version.



*     Agrega los 12 periodos para cada Clave:
         LOOP AT o_utiles->gt_period_var ASSIGNING FIELD-SYMBOL(<fs_perio>).

           CLEAR: wa_result-/bic/zpl_coef, wa_result-/bic/zpl_gasto, wa_result-/bic/zpl_gastp, wa_result-/bic/zpl_part,
                   wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp, wa_result-/bic/zquantity.


*----------------------------------------*
****   PERIODOS REALES:
*----------------------------------------*

           IF <fs_perio>-tipo EQ 'R'.
             READ TABLE  lt_gastos_re ASSIGNING FIELD-SYMBOL(<fs_gasto>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                              fiscper    = <fs_perio>-period
                                                              gl_account = <fs_clave>-gl_account
                                                              costcenter = <fs_clave>-costcenter
                                                              coorder    = <fs_clave>-coorder
                                                          BINARY SEARCH.

             IF sy-subrc EQ 0.
               wa_result-/bic/zpl_gasto = <fs_gasto>-gastos * 100.
             ENDIF.




*-----------------------------
*        VENTAS REALES:
*----------------------------
             READ TABLE lt_vta_real_s1 ASSIGNING FIELD-SYMBOL(<fs_venta_s1>) WITH KEY  comp_code = <fs_clave>-comp_code
                                                                              fiscper   = <fs_perio>-period
                                                                              sucursal  = <fs_cost>-sales_off
                                                                              canal     = <fs_cost>-distr_chan
                                                                              linea     = <fs_cost>-matl_group
                                                                              marca     = <fs_cost>-/bic/zmar
                                                                              BINARY SEARCH.

             IF sy-subrc EQ 0.
               wa_result-/bic/zpl_venta = <fs_venta_s1>-ventastotal * 100.

             ELSE.
*          Busca las ventas agrupadas por  Sucursal, Canal, Linea
               READ TABLE lt_vta_real_s2 ASSIGNING FIELD-SYMBOL(<fs_venta_s2>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                            fiscper   = <fs_perio>-period
                                                                            sucursal  = <fs_cost>-sales_off
                                                                            canal     = <fs_cost>-distr_chan
                                                                            linea     = <fs_cost>-matl_group
                                                                            BINARY SEARCH.
               IF sy-subrc EQ 0.
                 wa_result-/bic/zpl_venta = <fs_venta_s2>-ventastotal * 100.
               ELSE.
*            Busca las ventas agrupadas por sucursal, canal
                 READ TABLE lt_vta_real_s3 ASSIGNING FIELD-SYMBOL(<fs_venta_s3>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                              fiscper   = <fs_perio>-period
                                                                              sucursal  = <fs_cost>-sales_off
                                                                              canal     = <fs_cost>-distr_chan
                                                                              BINARY SEARCH.
                 IF sy-subrc EQ 0.
                   wa_result-/bic/zpl_venta = <fs_venta_s3>-ventastotal * 100.

                 ELSE.
*            Busca las ventas agrupadas por sucursal
                   READ TABLE lt_vta_real_s4 ASSIGNING FIELD-SYMBOL(<fs_venta_s4>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                                fiscper   = <fs_perio>-period
                                                                                sucursal  = <fs_cost>-sales_off
                                                                                BINARY SEARCH.
                   IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN o_utiles->gt_cuentas.
                     wa_result-/bic/zpl_venta = <fs_venta_s4>-ventastotal * 100.
                   ENDIF."Sucursal
                 ENDIF."Sucursal, Canal
               ENDIF."Sucursal, Canal, LInea
             ENDIF."Sucursal, Linea Marca



           ELSE. "Parte Presupuesto, 23V0 por ejemplo
*----------------------------------------*
* PERIODOS RESUPUESTO
*----------------------------------------*
* Gasto Presupuesto 23V0
             READ TABLE  lt_gastos_pre ASSIGNING <fs_gasto>  WITH KEY comp_code  = <fs_clave>-comp_code
                                                              fiscper    = <fs_perio>-period
                                                              gl_account = <fs_clave>-gl_account
                                                              costcenter = <fs_clave>-costcenter
                                                              coorder    = <fs_clave>-coorder
                                                          BINARY SEARCH.
             IF sy-subrc EQ 0.
               wa_result-/bic/zpl_gasto = <fs_gasto>-gastos.
             ENDIF.


*Venta COPA Presupuesto 23V0
             READ TABLE lt_vta_pres_s1 ASSIGNING FIELD-SYMBOL(<fs_venta_p1>)  WITH KEY  comp_code = <fs_clave>-comp_code
                                                                                  fiscper   = <fs_perio>-period
                                                                                  sucursal  = <fs_cost>-sales_off
                                                                                  canal     = <fs_cost>-distr_chan
                                                                                  linea     = <fs_cost>-matl_group
                                                                                  marca     = <fs_cost>-/bic/zmar
                                                                                  BINARY SEARCH.

             IF sy-subrc EQ 0.
               wa_result-/bic/zpl_venta = <fs_venta_p1>-ventastotal * 100.

             ELSE.
*          Busca las ventas agrupadas por  Sucursal, Canal, Linea
               READ TABLE lt_vta_pres_s2 ASSIGNING FIELD-SYMBOL(<fs_venta_p2>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                            fiscper   = <fs_perio>-period
                                                                            sucursal  = <fs_cost>-sales_off
                                                                            canal     = <fs_cost>-distr_chan
                                                                            linea     = <fs_cost>-matl_group
                                                                            BINARY SEARCH.
               IF sy-subrc EQ 0.
                 wa_result-/bic/zpl_venta = <fs_venta_p2>-ventastotal * 100.
               ELSE.
*            Busca las ventas agrupadas por sucursal, canal
                 READ TABLE lt_vta_pres_s3 ASSIGNING FIELD-SYMBOL(<fs_venta_p3>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                              fiscper   = <fs_perio>-period
                                                                              sucursal  = <fs_cost>-sales_off
                                                                              canal     = <fs_cost>-distr_chan
                                                                              BINARY SEARCH.
                 IF sy-subrc EQ 0.
                   wa_result-/bic/zpl_venta = <fs_venta_p3>-ventastotal * 100.

                 ELSE.
*            Busca las ventas agrupadas por sucursal
                   READ TABLE lt_vta_pres_s4 ASSIGNING FIELD-SYMBOL(<fs_venta_p4>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                                fiscper   = <fs_perio>-period
                                                                                sucursal  = <fs_cost>-sales_off
                                                                                BINARY SEARCH.
                   IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN o_utiles->gt_cuentas.
                     wa_result-/bic/zpl_venta = <fs_venta_p4>-ventastotal * 100.
                   ENDIF."Sucursal
                 ENDIF."Sucursal, Canal
               ENDIF."Sucursal, Canal, LInea
             ENDIF."Sucursal, Linea Marca

           ENDIF."REAL/PRESUP

           wa_result-/bic/zpl_venta = abs( wa_result-/bic/zpl_venta  ).
           wa_result-/bic/zpl_gasto = abs( wa_result-/bic/zpl_gasto  ).

           IF wa_result-/bic/zpl_venta <> 0.
             wa_result-/bic/zpl_coef =  wa_result-/bic/zpl_gasto / wa_result-/bic/zpl_venta.
           ELSE.
             wa_result-/bic/zpl_coef = 0.
           ENDIF.


*----------------------------------------*
* VENTA RESUPUESTADA (24V0)
*----------------------------------------*
           READ TABLE lt_vta_pres_s1 ASSIGNING <fs_venta_p1>  WITH KEY  comp_code = <fs_clave>-comp_code
                                                                                 fiscper   = <fs_perio>-plan
                                                                                 sucursal  = <fs_cost>-sales_off
                                                                                 canal     = <fs_cost>-distr_chan
                                                                                 linea     = <fs_cost>-matl_group
                                                                                 marca     = <fs_cost>-/bic/zmar
                                                                                 BINARY SEARCH.

           IF sy-subrc EQ 0.
             wa_result-/bic/zpl_ventp =  abs( <fs_venta_p1>-ventastotal * 100 ).

           ELSE.
*          Busca las ventas agrupadas por  Sucursal, Canal, Linea
             READ TABLE lt_vta_pres_s2 ASSIGNING <fs_venta_p2> WITH KEY comp_code = <fs_clave>-comp_code
                                                                           fiscper   = <fs_perio>-plan
                                                                          sucursal  = <fs_cost>-sales_off
                                                                          canal     = <fs_cost>-distr_chan
                                                                          linea     = <fs_cost>-matl_group
                                                                          BINARY SEARCH.
             IF sy-subrc EQ 0.
               wa_result-/bic/zpl_ventp = <fs_venta_p2>-ventastotal * 100.
             ELSE.
*            Busca las ventas agrupadas por sucursal, canal
               READ TABLE lt_vta_pres_s3 ASSIGNING <fs_venta_p3> WITH KEY comp_code = <fs_clave>-comp_code
                                                                            fiscper   = <fs_perio>-plan
                                                                            sucursal  = <fs_cost>-sales_off
                                                                            canal     = <fs_cost>-distr_chan
                                                                            BINARY SEARCH.
               IF sy-subrc EQ 0.
                 wa_result-/bic/zpl_ventp = <fs_venta_p3>-ventastotal * 100.

               ELSE.
*            Busca las ventas agrupadas por sucursal
                 READ TABLE lt_vta_pres_s4 ASSIGNING <fs_venta_p4>  WITH KEY comp_code = <fs_clave>-comp_code
                                                                              fiscper   = <fs_perio>-plan
                                                                              sucursal  = <fs_cost>-sales_off
                                                                              BINARY SEARCH.
                 IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN o_utiles->gt_cuentas.
                   wa_result-/bic/zpl_ventp = <fs_venta_p4>-ventastotal * 100.
                 ENDIF."Sucursal
               ENDIF."Sucursal, Canal
             ENDIF."Sucursal, Canal, LInea
           ENDIF."Sucursal, Linea Marca



*--------------------------------------------------------
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
               i_tipo   = 'VAR'
               i_period = <fs_perio>-period "Periodo de input/precarga
             IMPORTING
               o_plan   = wa_result-fiscper. "Periodo Plan


           wa_result-fiscyear = wa_result-fiscper(4).

           APPEND wa_result TO lt_aux.

         ENDLOOP." Periodos
       ENDLOOP." Claves Source Package

       result_package[] = lt_aux[].




     CATCH  cx_root INTO DATA(l_excepcion).

   ENDTRY.
