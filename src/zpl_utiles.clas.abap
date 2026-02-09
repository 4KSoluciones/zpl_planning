CLASS zpl_utiles DEFINITION   PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      c_block_default TYPE c VALUE 'X',
      c_unblock_act   TYPE c VALUE ' ', "Desbloquea a Todos los usuarios y todos los reportes

      c_init_gastos   TYPE c VALUE ' ',

      c_pxq           TYPE /bic/oizpl_link VALUE 'PXQ',
      c_var           TYPE /bic/oizpl_link VALUE 'VAR',
      c_varq          TYPE /bic/oizpl_link VALUE 'VARQ',
      c_inv           TYPE /bic/oizpl_link VALUE 'INV',
      c_test_user     TYPE /bi0/oiusername VALUE 'JUACOLMA', "Usuario de Test para ZPL_USER_UE

      c_tc            TYPE /bic/oizpl_tcin VALUE '7300',
      c_version       TYPE /bi0/oiversion VALUE '001', "Nueva versión 26V0, Version 1
      c_draft         TYPE /bi0/oiversion VALUE '000', "Al momento de la precarga no hay Version Asignada
      c_plan          TYPE /bic/oiztabla VALUE '26V0', "Nueva version pasaría a ser 26V0 -> luego volver a 25V0
      c_plan_actual   TYPE /bic/oiztabla VALUE '25V0', "Siguiente año cambiar a 26V0
      c_carga         TYPE /bic/oiztabla VALUE 'R3',
      c_actual        TYPE /bic/oiztabla VALUE 'R3',
      c_forecast      TYPE /bic/oiztabla VALUE '25VF',
*------------------------------------------------------------------
* Configuració FORECAST
      c_version_fore  TYPE /bi0/oiversion VALUE '011', "Versión de Forecast a Generar/Actualizar
      c_version_ibm   TYPE /bi0/oiversion VALUE 'F12', "Versión IBM de Ventas Forecast
      c_low           TYPE /bi0/oifiscper VALUE '2025012', "Rango Forecast: Desde (año mes)
      c_high          TYPE /bi0/oifiscper VALUE '2025012', "Rango Forecast: Hasta (año mes)
      c_fore_low      TYPE /bi0/oicalmonth VALUE '202512', "Rango Forecast: Desde (año mes)
      c_fore_high     TYPE /bi0/oicalmonth VALUE '202512', "Rango Forecast: Hasta (año mes)
      c_real_low      TYPE /bi0/oicalmonth VALUE '202501', "Rango Reales: Desde (año mes)
      c_real_high     TYPE /bi0/oicalmonth VALUE '202511', "Rango Reales: Hasta (año mes)
*------------------------------------------------------------------
* Constantes para creación de Combinaciones
      c_unit          TYPE /bi0/oiunit VALUE 'ST',
      c_pyg           TYPE /bi0/oicurrency VALUE 'PYG',
      c_curtype       TYPE /bi0/oicurtype  VALUE '10'.

*   Estructura Gastos (Real y Plan)
    TYPES:
      BEGIN OF ty_wa_result_pxq,
        comp_code      TYPE /bi0/oicomp_code,
        co_area        TYPE /bi0/oico_area,
        fiscvarnt      TYPE /bi0/oifiscvarnt,
        fiscyear       TYPE /bi0/oifiscyear,
        fiscper        TYPE /bi0/oifiscper,
        chrt_accts     TYPE /bi0/oichrt_accts,
        version        TYPE /bi0/oiversion,
        gl_account     TYPE /bi0/oigl_account,
        costcenter     TYPE /bi0/oicostcenter,
        coorder        TYPE /bi0/oicoorder,
        /bic/ztabla    TYPE /bic/oiztabla,
        unit           TYPE /bi0/oiunit,
        currency       TYPE /bi0/oicurrency,
        curtype        TYPE /bi0/oicurtype,
        username       TYPE /bi0/oiusername,
        /bic/zpl_upric TYPE /bic/oizpl_upric,
        /bic/zpl_qinpu TYPE /bic/oizpl_qinpu,
        /bic/zpl_gasto TYPE /bic/oizpl_gasto,
        /bic/zquantity TYPE /bic/oizquantity,
        /bic/zpl_pgral TYPE /bic/oizpl_pgral,
        /bic/zpl_pfila TYPE /bic/oizpl_pfila,
        record         TYPE rsarecord,
      END   OF ty_wa_result_pxq.
    TYPES:
      ty_lt_result_pxq      TYPE STANDARD TABLE OF ty_wa_result_pxq
                        WITH NON-UNIQUE DEFAULT KEY.


    TYPES:
      BEGIN OF ty_wa_result_vta,
        comp_code      TYPE /bi0/oicomp_code,
        co_area        TYPE /bi0/oico_area,
        username       TYPE /bi0/oiusername,
        fiscper        TYPE /bi0/oifiscper,

        gl_account     TYPE /bi0/oigl_account,
        costcenter     TYPE /bi0/oicostcenter,
        coorder        TYPE /bi0/oicoorder,

        /bic/ztabla    TYPE /bic/oiztabla,
        version        TYPE /bi0/oiversion,

        fiscvarnt      TYPE /bi0/oifiscvarnt,
        chrt_accts     TYPE /bi0/oichrt_accts,
        currency       TYPE /bi0/oicurrency,

        unit           TYPE /bi0/oiunit,
        fiscyear       TYPE /bi0/oifiscyear,
        recordmode     TYPE rodmupdmod,

        sales_off      TYPE /bi0/oisales_off,
        matl_group     TYPE /bi0/oimatl_group,
        /bic/zmar      TYPE /bic/oizmar,

        /bic/zpl_venta TYPE /bic/oizpl_venta,
        /bic/zpl_ventp TYPE /bic/oizpl_ventp,
        /bic/zpl_ventq TYPE /bic/oizpl_vtaq,
        /bic/zpl_vtaqp TYPE /bic/oizpl_vtaqp,
        record         TYPE rsarecord,
      END   OF ty_wa_result_vta.
    TYPES:
      ty_lt_result_vta TYPE STANDARD TABLE OF ty_wa_result_vta
                        WITH NON-UNIQUE DEFAULT KEY,


      BEGIN OF ty_wa_result_var,
        comp_code      TYPE /bi0/oicomp_code,
        co_area        TYPE /bi0/oico_area,
        fiscvarnt      TYPE /bi0/oifiscvarnt,
        fiscyear       TYPE /bi0/oifiscyear,
        fiscper        TYPE /bi0/oifiscper,
        chrt_accts     TYPE /bi0/oichrt_accts,
        version        TYPE /bi0/oiversion,
        gl_account     TYPE /bi0/oigl_account,
        costcenter     TYPE /bi0/oicostcenter,
        coorder        TYPE /bi0/oicoorder,
        /bic/ztabla    TYPE /bic/oiztabla,
        unit           TYPE /bi0/oiunit,
        recordmode     TYPE rodmupdmod,
        sales_off      TYPE /bi0/oisales_off,
        matl_group     TYPE /bi0/oimatl_group,
        /bic/zmar      TYPE /bic/oizmar,
        username       TYPE /bi0/oiusername,
        /bic/zpl_coef  TYPE /bic/oizpl_coef,
        /bic/zpl_part  TYPE /bic/oizpl_part,
        /bic/zpl_venta TYPE /bic/oizpl_venta,
        /bic/zpl_gastp TYPE /bic/oizpl_gastp,
        /bic/zpl_ventp TYPE /bic/oizpl_ventp,
        /bic/zpl_gasto TYPE /bic/oizpl_gasto,
        /bic/zpl_cofin TYPE /bic/oizpl_cofin,
        currency       TYPE /bi0/oicurrency,
        /bic/zquantity TYPE /bic/oizquantity,
        record         TYPE rsarecord,
      END   OF ty_wa_result_var.
    TYPES:
      ty_lt_result_var  TYPE STANDARD TABLE OF ty_wa_result_var
                        WITH NON-UNIQUE DEFAULT KEY.

    TYPES:
      BEGIN OF ty_wa_result_varq,
        comp_code      TYPE /bi0/oicomp_code,
        co_area        TYPE /bi0/oico_area,
        fiscvarnt      TYPE /bi0/oifiscvarnt,
        fiscyear       TYPE /bi0/oifiscyear,
        fiscper        TYPE /bi0/oifiscper,
        chrt_accts     TYPE /bi0/oichrt_accts,
        version        TYPE /bi0/oiversion,
        gl_account     TYPE /bi0/oigl_account,
        costcenter     TYPE /bi0/oicostcenter,
        coorder        TYPE /bi0/oicoorder,
        /bic/ztabla    TYPE /bic/oiztabla,
        unit           TYPE /bi0/oiunit,
        recordmode     TYPE rodmupdmod,
        sales_off      TYPE /bi0/oisales_off,
        matl_group     TYPE /bi0/oimatl_group,
        /bic/zmar      TYPE /bic/oizmar,
        currency       TYPE /bi0/oicurrency,
        username       TYPE /bi0/oiusername,
        /bic/zpl_venta TYPE /bic/oizpl_venta,
        /bic/zpl_ventp TYPE /bic/oizpl_ventp,
        /bic/zpl_vtaq  TYPE /bic/oizpl_vtaq,
        /bic/zpl_vtaqp TYPE /bic/oizpl_vtaqp,
        /bic/zpl_qinpu TYPE /bic/oizpl_qinpu,
        /bic/zpl_upric TYPE /bic/oizpl_upric,
        /bic/zpl_gasto TYPE /bic/oizpl_gasto,
        /bic/zpl_gastq TYPE /bic/oizpl_gastq,
      END   OF ty_wa_result_varq,
      ty_lt_result_varq TYPE STANDARD TABLE OF ty_wa_result_varq
                        WITH NON-UNIQUE DEFAULT KEY.


    TYPES: BEGIN OF ty_wa_clave_venta,
             co_area    TYPE /bi0/oico_area,
             costcenter TYPE /bi0/oicostcenter,
             sales_off  TYPE /bi0/oisales_off,
             matl_group TYPE /bi0/oimatl_group,
             /bic/zmar  TYPE /bic/oizmar,
           END OF ty_wa_clave_venta,
           ty_lt_clave_venta TYPE SORTED TABLE OF ty_wa_clave_venta WITH NON-UNIQUE KEY co_area sales_off matl_group /bic/zmar.

    TYPES: BEGIN OF ty_wa_clave_venta_2000,
             co_area    TYPE /bi0/oico_area,
             costcenter TYPE /bi0/oicostcenter,
             sales_off  TYPE /bi0/oisales_off,
             distr_chan TYPE /bi0/oidistr_chan,
             matl_group TYPE /bi0/oimatl_group,
             /bic/zmar  TYPE /bic/oizmar,
           END OF ty_wa_clave_venta_2000,
           ty_lt_clave_venta_2000 TYPE SORTED TABLE OF ty_wa_clave_venta_2000 WITH NON-UNIQUE KEY co_area sales_off distr_chan matl_group /bic/zmar.



    TYPES: BEGIN OF ty_wa_clave,
             comp_code  TYPE /bi0/oicomp_code,
             co_area    TYPE /bi0/oico_area,
             gl_account TYPE /bi0/oigl_account,
             costcenter TYPE /bi0/oicostcenter,
             coorder    TYPE /bi0/oicoorder,
             username   TYPE /bi0/oiusername,
           END OF ty_wa_clave,
           ty_lt_claves     TYPE SORTED TABLE OF ty_wa_clave WITH NON-UNIQUE KEY
              comp_code co_area gl_account costcenter coorder,
           ty_lt_claves_aux TYPE TABLE OF ty_wa_clave.


    TYPES: BEGIN OF ty_wa_periodos,
             actual TYPE /bi0/oifiscper,
             plan   TYPE /bi0/oifiscper,
             tipo   TYPE char1, "R: Real, P:Plan
           END OF ty_wa_periodos,
           ty_gt_periodos TYPE SORTED TABLE OF ty_wa_periodos WITH
                            UNIQUE KEY actual.

    TYPES: BEGIN OF ty_wa_map,
             in  TYPE char14,
             out TYPE char14,
           END OF ty_wa_map,
           ty_lt_map TYPE STANDARD TABLE OF ty_wa_map.

    TYPES: BEGIN OF ty_wa_desicion,
             orden  TYPE i,
             cuenta TYPE i,
             cebe   TYPE i,
             resp   TYPE char1,
           END OF ty_wa_desicion,
           ty_gt_desicion TYPE HASHED TABLE OF ty_wa_desicion WITH
                            UNIQUE KEY orden cebe cuenta,

           BEGIN OF ty_wa_user,
             user_ibm TYPE char3,
             user_sap TYPE char12,
           END OF ty_wa_user,
           ty_gt_user TYPE STANDARD TABLE OF ty_wa_user
       WITH NON-UNIQUE KEY primary_key     COMPONENTS user_ibm
       WITH NON-UNIQUE SORTED KEY sap_key  COMPONENTS user_sap.



    DATA: g_tipo_actual        TYPE c VALUE 'R', "Toma el periodo para el actual/real desde el Actual o bien el Plan
          g_ztabla             TYPE /bic/oiztabla VALUE c_plan,
          g_plan_version       TYPE /bi0/oiversion VALUE c_version,
          g_draft_version      TYPE num03 VALUE '000',

          gt_desicion          TYPE ty_gt_desicion,
          gt_fields_pxq        TYPE ty_lt_map,
          gt_fields_var        TYPE ty_lt_map,
          gt_fields_varq       TYPE ty_lt_map,
          gt_fields_abm        TYPE ty_lt_map,
          gt_user              TYPE ty_gt_user,
          lt_users             TYPE ty_gt_user,
          gt_period_pxq        TYPE ty_gt_periodos,
          gt_period_var        TYPE ty_gt_periodos,
          gt_period_var_2000   TYPE ty_gt_periodos,
          gt_period_varq       TYPE ty_gt_periodos,
          gt_period_varq_2000  TYPE ty_gt_periodos,
          gt_period_vta        TYPE ty_gt_periodos,
          gt_period_plan       TYPE ty_gt_periodos,
          gt_cuentas           TYPE RANGE OF /bi0/oigl_account,
          lt_orden             TYPE ty_lt_claves,
          lt_cebe              TYPE ty_lt_claves,
          lt_cuenta            TYPE ty_lt_claves,
          gt_claves            TYPE ty_lt_claves,
          g_users              TYPE ty_wa_user,
          gr_orden             TYPE RANGE OF /bi0/oicoorder,
          gr_cebe              TYPE RANGE OF /bi0/oiprofit_ctr,
          gr_cuenta            TYPE RANGE OF /bi0/oigl_account,
          gr_fiscper_pxq       TYPE RANGE OF /bi0/oifiscper,
          gr_fiscper_var       TYPE RANGE OF /bi0/oifiscper,
          gr_fiscper_var_2000  TYPE RANGE OF /bi0/oifiscper,
          gr_fiscper_varq      TYPE RANGE OF /bi0/oifiscper,
          gr_fiscper_varq_2000 TYPE RANGE OF /bi0/oifiscper,

          gr_fiscyear          TYPE RANGE OF /bi0/oifiscyear,
          gr_user_varq         TYPE RANGE OF /bi0/oiusername,
          gr_user_pxq          TYPE RANGE OF /bi0/oiusername,
          gr_zvar              TYPE RANGE OF /bi0/mcoorder-/bic/zmar,
          gr_zpxq              TYPE RANGE OF /bi0/mcoorder-/bic/zmar,
          gr_zvarq             TYPE RANGE OF /bi0/mcoorder-/bic/zmar.

**********************************************************************
    CLASS-METHODS: get_version EXPORTING o_version TYPE /bi0/oiversion,
      get_version_fore EXPORTING o_version TYPE /bi0/oiversion,
      get_forecast_table EXPORTING o_table TYPE /bic/oiztabla.

    METHODS: constructor,
      get_plan_version EXPORTING o_plan_version TYPE /bi0/oiversion,
      get_tabla  EXPORTING o_tabla TYPE /bic/oiztabla,

      get_pxq   IMPORTING i_claves TYPE ty_lt_claves_aux
                EXPORTING e_result TYPE ty_lt_result_pxq,

      get_pxq_pc   IMPORTING i_claves  TYPE ty_lt_claves_aux i_version TYPE /bi0/oiversion i_tabla TYPE /bic/oiztabla
                   EXPORTING e_result  TYPE ty_lt_result_pxq,


      get_pxq_init   IMPORTING i_claves TYPE ty_lt_claves_aux
                     EXPORTING e_result TYPE ty_lt_result_pxq,

      get_var   IMPORTING i_claves TYPE ty_lt_claves_aux
                EXPORTING e_result TYPE ty_lt_result_var,

      get_vta_actual   IMPORTING i_claves TYPE ty_lt_claves_aux
                       EXPORTING e_result TYPE ty_lt_result_vta,

      get_vta_presup  IMPORTING i_claves TYPE ty_lt_claves_aux i_tabla type /bic/oiztabla OPTIONAL
                      EXPORTING e_result TYPE ty_lt_result_vta,

      get_vta_fore_1000  IMPORTING i_claves  TYPE ty_lt_claves_aux
                                   i_version TYPE /bi0/oiversion
                         EXPORTING e_result  TYPE ty_lt_result_vta,
      get_vta_fore_2000   IMPORTING i_claves  TYPE ty_lt_claves_aux
                                    i_version TYPE /bi0/oiversion
                          EXPORTING e_result  TYPE ty_lt_result_vta,
      get_vta_forecast  IMPORTING i_claves  TYPE ty_lt_claves_aux
                                  i_version TYPE /bi0/oiversion
                        EXPORTING e_result  TYPE ty_lt_result_vta,

      get_vta_2000   IMPORTING i_claves TYPE ty_lt_claves_aux
                     EXPORTING e_result TYPE ty_lt_result_vta,

      get_var_2000   IMPORTING i_claves TYPE ty_lt_claves_aux
                     EXPORTING e_result TYPE ty_lt_result_var,

      get_var_init   IMPORTING i_claves TYPE ty_lt_claves_aux
                     EXPORTING e_result TYPE ty_lt_result_var,

      get_varq   IMPORTING i_claves TYPE ty_lt_claves_aux
                 EXPORTING e_result TYPE ty_lt_result_varq,


      get_varq_2000   IMPORTING i_claves TYPE ty_lt_claves_aux
                      EXPORTING e_result TYPE ty_lt_result_varq,

      get_varq_gasto_actual   IMPORTING i_claves TYPE ty_lt_claves_aux
                              EXPORTING e_result TYPE ty_lt_result_varq,

      get_var_gasto_actual   IMPORTING i_claves TYPE ty_lt_claves_aux
                             EXPORTING e_result TYPE ty_lt_result_var,

      get_pxq_gasto_actual   IMPORTING i_claves TYPE ty_lt_claves_aux
                             EXPORTING e_result TYPE ty_lt_result_pxq,


      get_varq_init   IMPORTING i_claves TYPE ty_lt_claves_aux
                      EXPORTING e_result TYPE ty_lt_result_varq,

      get_locked_entries IMPORTING i_s_data  TYPE any
                                   i_link    TYPE /bic/oizpl_link
                         EXPORTING e_s_mesg  TYPE if_rspls_cr_types=>tn_s_mesg
                                   e_noinput TYPE rs_bool,

      get_claves IMPORTING i_soc    TYPE char4
                           i_soco   TYPE char4
                           i_cuenta TYPE /sappo/tab_rseloption
                           i_cebe   TYPE /sappo/tab_rseloption
                           i_orden  TYPE /sappo/tab_rseloption
                           i_user   TYPE char12
                 EXPORTING o_claves TYPE ty_lt_claves
                           o_result TYPE  rs_bool
                           o_msg    TYPE if_rspls_cr_types=>tn_s_mesg,

      check_clave  IMPORTING i_soc    TYPE char4
                             i_soco   TYPE char4
                             i_cuenta TYPE char10
                             i_cebe   TYPE char10
                             i_orden  TYPE char12
                             i_user   TYPE char12
                   EXPORTING o_result TYPE  rs_bool
                             o_msg    TYPE if_rspls_cr_types=>tn_s_mesg,

      check_clave_abm  IMPORTING i_soc    TYPE char4
                                 i_soco   TYPE char4
                                 i_cuenta TYPE char10
                                 i_cebe   TYPE char10
                                 i_orden  TYPE char12
                                 i_user   TYPE char12
                       EXPORTING o_result TYPE  rs_bool
                                 o_ibm    TYPE char4
                                 o_msg    TYPE if_rspls_cr_types=>tn_s_mesg,


      derive_user   IMPORTING i_soc    TYPE char4
                              i_soco   TYPE char4
                              i_cuenta TYPE char10
                              i_cebe   TYPE char10
                              i_orden  TYPE char12
                    EXPORTING o_user   TYPE char12
                              o_result TYPE  rs_bool
                              o_msg    TYPE if_rspls_cr_types=>tn_s_mesg,

      get_resp      IMPORTING i_soc    TYPE char4
                              i_soco   TYPE char4 OPTIONAL
                              i_cuenta TYPE char10
                              i_cebe   TYPE char10
                              i_orden  TYPE char12
                    EXPORTING o_user   TYPE char12
                              o_result TYPE  rs_bool
                              o_msg    TYPE if_rspls_cr_types=>tn_s_mesg,

      get_ibm_user  IMPORTING i_sap_user TYPE char12
                    EXPORTING o_ibm_user TYPE char3,

      get_sap_user IMPORTING i_ibm_user TYPE char3
                   EXPORTING o_sap_user TYPE char12,


      get_plan_period IMPORTING i_period TYPE /bi0/oifiscper
                                i_tipo   TYPE char4
                      EXPORTING o_plan   TYPE /bi0/oifiscper,

      get_actual_period IMPORTING i_period TYPE /bi0/oifiscper
                                  i_tipo   TYPE char4
                        EXPORTING o_actual TYPE /bi0/oifiscper.




  PROTECTED SECTION.




ENDCLASS.



CLASS zpl_utiles IMPLEMENTATION.


  METHOD check_clave.


*  MPORTING i_soc    TYPE char4
*                             i_soco   TYPE char4
*                             i_cuenta TYPE char10
*                             i_cebe   TYPE char10
*                             i_orden  TYPE char12
*                             i_user   TYPE char12
*                   EXPORTING o_result TYPE  rs_bool
*                             o_msg    TYPE if_rspls_cr_types=>tn_s_mesg,
*

* data(lt_conv) = value #( codigo = DST   user = DAVSTEGE ).


    DATA: wa_clave   TYPE ty_wa_desicion,
          l_ibm_user TYPE char3.


*1) Busca los Técnicos correspondientes:




*1.2) CEBE
    SELECT SINGLE a~/bic/ztecnico, a~/bic/zabc
          INTO @DATA(l_user_cebe)
          FROM /bi0/mprofit_ctr AS a
          WHERE a~profit_ctr EQ @i_cebe
          AND a~co_area EQ @i_soco
          AND a~objvers EQ 'A'
          AND a~dateto EQ '99991231'.

    IF sy-subrc EQ 0 AND l_user_cebe-/bic/ztecnico IS NOT INITIAL.
      wa_clave-cebe = 1.
    ELSE.
      wa_clave-cebe = 0.
    ENDIF.

*1.2) CUENTA (tiene dependencia del CEBE en cuanto si es Fabrica)
    SELECT SINGLE
             /bic/ztec_cuen,
             /bic/ztec_cue2,
             /bic/ztec_cue3,
             /bic/ztec_cue4,
            /bic/ztec_cue5,
            /bic/ztec_cue6

             INTO @DATA(wa_cuenta)
             FROM /bi0/mgl_account AS a
             WHERE a~gl_account EQ @i_cuenta
             AND a~chrt_accts EQ 'SQS'
             AND a~objvers EQ 'A'.
    IF sy-subrc EQ 0.
      CASE i_soc.
        WHEN '1000'.
          IF l_user_cebe-/bic/zabc NE 'FABRICA'.
            IF (  wa_cuenta-/bic/ztec_cuen IS NOT INITIAL ).
              wa_clave-cuenta = 1.
            ELSE.
              wa_clave-cuenta = 0.
            ENDIF.
          ELSE.
            IF (  wa_cuenta-/bic/ztec_cue2 IS NOT INITIAL ).
              wa_clave-cuenta = 1.
            ELSE.
              wa_clave-cuenta = 0.
            ENDIF.

          ENDIF.
        WHEN '2000'.
          IF wa_cuenta-/bic/ztec_cue4 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.

        WHEN '3000'.
          IF wa_cuenta-/bic/ztec_cue3 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.

        WHEN '6000'.
          IF wa_cuenta-/bic/ztec_cue6 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.


        WHEN '7000'.
          IF wa_cuenta-/bic/ztec_cue5 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.

      ENDCASE.

    ENDIF.


*1.3) ORDEN
    SELECT SINGLE a~resp_user
     INTO @DATA(l_user_orden)
     FROM /bi0/mcoorder AS a
     WHERE a~coorder EQ @i_orden
     AND a~objvers EQ 'A'.

    IF sy-subrc EQ 0 AND l_user_orden IS NOT INITIAL.
      wa_clave-orden = 1.
    ELSE.
      wa_clave-orden = 0.
    ENDIF.






*2)  Busca el usuario IBM, segun el usuario SAP ingresado
    me->get_ibm_user(
      EXPORTING
        i_sap_user = i_user
      IMPORTING
        o_ibm_user = l_ibm_user
    ).

    IF l_ibm_user IS INITIAL.

*     Llenar mensaje de respuesta.
      o_msg-msgid = 'USUARIO'.
      o_msg-msgno = '1'.
      o_msg-msgty = 'E'.
      CONCATENATE ` ` ' Usuario IBM no encontrado para el usuario: ' ` ` i_user ` ` INTO o_msg-msgv1.
      o_msg-msgv2 = i_cuenta.
      o_msg-msgv3 = i_cebe.
      o_msg-msgv4 = i_orden.

      o_result = ' '.

      EXIT.
    ENDIF.


*3) Busca en la matriz de desición, cual es el Responsable a asignar
    READ TABLE gt_desicion ASSIGNING FIELD-SYMBOL(<fs_des>)
    WITH KEY orden = wa_clave-orden
             cebe = wa_clave-cebe
             cuenta = wa_clave-cuenta.

    IF sy-subrc EQ 0.

      wa_clave-resp = <fs_des>-resp.

      CASE wa_clave-resp.

        WHEN 'Y'."Valida Técnico CUENTA

          CASE i_soc.
            WHEN '1000'.
              IF wa_cuenta-/bic/ztec_cue2 EQ l_ibm_user.
                o_result = 'X'.
              ELSEIF wa_cuenta-/bic/ztec_cuen EQ l_ibm_user.
                o_result = 'X'.
              ELSE.

                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '1000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.

                o_result = ' '.
              ENDIF.
            WHEN '2000'.
              IF wa_cuenta-/bic/ztec_cue4 EQ l_ibm_user.
                o_result = 'X'.
              ELSE.

                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '2000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.
              ENDIF.

            WHEN '3000'.
              IF wa_cuenta-/bic/ztec_cue3 EQ l_ibm_user.
                o_result = 'X'.
              ELSE.
                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '3000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.

                o_result = ' '.
              ENDIF.

            WHEN '6000'.
              IF wa_cuenta-/bic/ztec_cue6 EQ l_ibm_user.
                o_result = 'X'.
              ELSE.
                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '6000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.
                o_result = ' '.
              ENDIF.


            WHEN '7000'.
              IF wa_cuenta-/bic/ztec_cue5 EQ l_ibm_user.
                o_result = 'X'.
              ELSE.
                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '7000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.
                o_result = ' '.
              ENDIF.

          ENDCASE.




        WHEN 'Z'."Valida Técnico CEBE
          IF l_user_cebe-/bic/ztecnico EQ l_ibm_user.
            o_result = 'X'.
*            completar mensaje
          ELSE.
            o_result = ' '.
            o_msg-msgid = 'CEBE'.
            o_msg-msgno = '1'.
            o_msg-msgty = 'E'.
            CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
            o_msg-msgv2 = i_cuenta.
            o_msg-msgv3 = i_cebe.
            o_msg-msgv4 = i_orden.

*            completar mensaje
          ENDIF.


        WHEN 'X'."Valida Técnico Orden
          IF l_user_orden EQ l_ibm_user.
            o_result = 'X'.
*            completar mensaje
          ELSE.
            o_msg-msgid = 'ORDEN'.
            o_msg-msgno = '1'.
            o_msg-msgty = 'E'.
            CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
            o_msg-msgv2 = i_cuenta.
            o_msg-msgv3 = i_cebe.
            o_msg-msgv4 = i_orden.

            o_result = ' '.
*            completar mensaje
          ENDIF.

        WHEN OTHERS.
          o_msg-msgid = 'CLAVE'.
          o_msg-msgno = '1'.
          o_msg-msgty = 'E'.
          CONCATENATE 'Clave sin responsable:' ` ` i_user ` ` INTO o_msg-msgv1.
          o_msg-msgv2 = i_cuenta.
          o_msg-msgv3 = i_cebe.
          o_msg-msgv4 = i_orden.

          o_result = ' '.




      ENDCASE.
    ELSE.
      "Clave no encontrada: mensaje

    ENDIF.

  ENDMETHOD.


  METHOD check_clave_abm.




    DATA: wa_clave   TYPE ty_wa_desicion,
          l_ibm_user TYPE char3.


*1) Busca los Técnicos correspondientes:




*1.2) CEBE
    SELECT SINGLE a~/bic/ztecnico, a~/bic/zabc
          INTO @DATA(l_user_cebe)
          FROM /bi0/mprofit_ctr AS a
          WHERE a~profit_ctr EQ @i_cebe
          AND a~co_area EQ @i_soco
          AND a~objvers EQ 'A'
          AND a~dateto EQ '99991231'.

    IF sy-subrc EQ 0 AND l_user_cebe-/bic/ztecnico IS NOT INITIAL.
      wa_clave-cebe = 1.
    ELSE.
      wa_clave-cebe = 0.
    ENDIF.

*1.2) CUENTA (tiene dependencia del CEBE en cuanto si es Fabrica)
    SELECT SINGLE
             /bic/ztec_cuen,
             /bic/ztec_cue2,
             /bic/ztec_cue3,
             /bic/ztec_cue4,
            /bic/ztec_cue5,
            /bic/ztec_cue6

             INTO @DATA(wa_cuenta)
             FROM /bi0/mgl_account AS a
             WHERE a~gl_account EQ @i_cuenta
             AND a~chrt_accts EQ 'SQS'
             AND a~objvers EQ 'A'.
    IF sy-subrc EQ 0.
      CASE i_soc.
        WHEN '1000'.
          IF l_user_cebe-/bic/zabc NE 'FABRICA'.
            IF (  wa_cuenta-/bic/ztec_cuen IS NOT INITIAL ).
              wa_clave-cuenta = 1.
            ELSE.
              wa_clave-cuenta = 0.
            ENDIF.
          ELSE.
            IF (  wa_cuenta-/bic/ztec_cue2 IS NOT INITIAL ).
              wa_clave-cuenta = 1.
            ELSE.
              wa_clave-cuenta = 0.
            ENDIF.

          ENDIF.
        WHEN '2000'.
          IF wa_cuenta-/bic/ztec_cue4 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.

        WHEN '3000'.
          IF wa_cuenta-/bic/ztec_cue3 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.

        WHEN '6000'.
          IF wa_cuenta-/bic/ztec_cue6 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.


        WHEN '7000'.
          IF wa_cuenta-/bic/ztec_cue5 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.

      ENDCASE.

    ENDIF.


*1.3) ORDEN
    SELECT SINGLE a~resp_user
     INTO @DATA(l_user_orden)
     FROM /bi0/mcoorder AS a
     WHERE a~coorder EQ @i_orden
     AND a~objvers EQ 'A'.

    IF sy-subrc EQ 0 AND l_user_orden IS NOT INITIAL.
      wa_clave-orden = 1.
    ELSE.
      wa_clave-orden = 0.
    ENDIF.






*2)  Busca el usuario IBM, segun el usuario SAP ingresado
    me->get_ibm_user(
      EXPORTING
        i_sap_user = i_user
      IMPORTING
        o_ibm_user = l_ibm_user
    ).

    IF l_ibm_user IS INITIAL.

*     Llenar mensaje de respuesta.
      o_msg-msgid = 'USUARIO'.
      o_msg-msgno = '1'.
      o_msg-msgty = 'E'.
      CONCATENATE ` ` ' Usuario IBM no encontrado para el usuario: ' ` ` i_user ` ` INTO o_msg-msgv1.
      o_msg-msgv2 = i_cuenta.
      o_msg-msgv3 = i_cebe.
      o_msg-msgv4 = i_orden.

      o_result = ' '.

      EXIT.
    ENDIF.



*3) Busca en la matriz de desición, cual es el Responsable a asignar
    READ TABLE gt_desicion ASSIGNING FIELD-SYMBOL(<fs_des>)
    WITH KEY orden = wa_clave-orden
             cebe = wa_clave-cebe
             cuenta = wa_clave-cuenta.

    IF sy-subrc EQ 0.

      wa_clave-resp = <fs_des>-resp.

      CASE wa_clave-resp.

        WHEN 'Y'."Valida Técnico CUENTA

          CASE i_soc.
            WHEN '1000'.
              IF wa_cuenta-/bic/ztec_cue2 EQ l_ibm_user.
                o_result = 'X'.
                o_ibm = l_ibm_user.
              ELSEIF wa_cuenta-/bic/ztec_cuen EQ l_ibm_user.
                o_result = 'X'.
                o_ibm = l_ibm_user.
              ELSE.
                o_ibm = wa_cuenta-/bic/ztec_cuen.
                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '1000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.

                o_result = ' '.
              ENDIF.
            WHEN '2000'.
              IF wa_cuenta-/bic/ztec_cue4 EQ l_ibm_user.
                o_result = 'X'.
                o_ibm = l_ibm_user.
              ELSE.
                o_ibm = wa_cuenta-/bic/ztec_cue4.
                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '2000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.
              ENDIF.

            WHEN '3000'.
              IF wa_cuenta-/bic/ztec_cue3 EQ l_ibm_user.
                o_result = 'X'.
                o_ibm = l_ibm_user.
              ELSE.
                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '3000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.
                o_ibm = wa_cuenta-/bic/ztec_cue3.
              ENDIF.

            WHEN '6000'.
              IF wa_cuenta-/bic/ztec_cue6 EQ l_ibm_user.
                o_result = 'X'.
                o_ibm = l_ibm_user.
              ELSE.
                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '6000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.
                o_ibm = wa_cuenta-/bic/ztec_cue6.
              ENDIF.


            WHEN '7000'.
              IF wa_cuenta-/bic/ztec_cue5 EQ l_ibm_user.
                o_result = 'X'.
                o_ibm = l_ibm_user.
              ELSE.
                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '7000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.
                o_ibm = wa_cuenta-/bic/ztec_cue5.
              ENDIF.

          ENDCASE.




        WHEN 'Z'."Valida Técnico CEBE
          IF l_user_cebe-/bic/ztecnico EQ l_ibm_user.
            o_result = 'X'.
            o_ibm = l_ibm_user.
*            completar mensaje
          ELSE.
            o_result = ' '.
            o_ibm = l_user_cebe-/bic/ztecnico.
            o_msg-msgid = 'CEBE'.
            o_msg-msgno = '1'.
            o_msg-msgty = 'E'.
            CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
            o_msg-msgv2 = i_cuenta.
            o_msg-msgv3 = i_cebe.
            o_msg-msgv4 = i_orden.

*            completar mensaje
          ENDIF.


        WHEN 'X'."Valida Técnico Orden
          IF l_user_orden EQ l_ibm_user.
            o_result = 'X'.
            o_ibm = l_ibm_user.
*            completar mensaje
          ELSE.
            o_msg-msgid = 'ORDEN'.
            o_msg-msgno = '1'.
            o_msg-msgty = 'E'.
            CONCATENATE 'Combinacion no valida:' ` ` i_user ` ` INTO o_msg-msgv1.
            o_msg-msgv2 = i_cuenta.
            o_msg-msgv3 = i_cebe.
            o_msg-msgv4 = i_orden.
            o_result = ' '.
            o_ibm = l_user_orden.
*            completar mensaje
          ENDIF.

        WHEN OTHERS.
          o_msg-msgid = 'CLAVE'.
          o_msg-msgno = '1'.
          o_msg-msgty = 'E'.
          CONCATENATE 'Clave sin responsable:' ` ` i_user ` ` INTO o_msg-msgv1.
          o_msg-msgv2 = i_cuenta.
          o_msg-msgv3 = i_cebe.
          o_msg-msgv4 = i_orden.
          o_result = ' '.

      ENDCASE.
    ELSE.
      "Clave no encontrada: mensaje

    ENDIF.








  ENDMETHOD.


  METHOD constructor.

    gr_user_varq = VALUE #( ( sign = 'I' option = 'EQ' low = 'GUSLACOU' ) ).

    gt_fields_pxq = VALUE #(  ( in = 'ZPL_DUM01'  out = '/bic/zpl_dum01' )
                            ( in = '0INFOPROV'   out = 'ZPL_A01' )
                            ( in = '0USERNAME'      out = 'username' )
                            ( in = '0FISCPER'      out = 'fiscper' )
                            ( in = '0FISCVARNT'     out = 'fiscvarnt' )
                            ( in = '0FISCYEAR'      out = 'fiscyear' )
                            ( in = '0CHRT_ACCTS'    out = 'chrt_accts' )
                            ( in = '0COORDER'       out = 'coorder' )
                            ( in = '0COSTCENTER'    out = 'costcenter' )
                            ( in = '0CO_AREA'       out = 'co_area' )
                            ( in = '0CURTYPE'       out = 'curtype ' )
                            ( in = '0GL_ACCOUNT'    out = 'gl_account' )
                            ( in = '0VERSION'       out = 'version' )
                            ( in = 'ZPLCOMPCO'      out = 'comp_code' )
                            ( in = 'ZMAR'           out = '/bic/zmar' )
                            ( in = 'ZTABLA'         out = '/bic/ztabla' )
                            ( in = '0CURRENCY'      out = 'currency' )
                            ( in = '0CURTYPE'       out = 'curtype' )
                            ( in = '0UNIT'          out = 'unit' )
                            ( in = 'ZPL_GASTO'  out = '/bic/zpl_gasto' )
                            ( in = 'ZPL_PFILA'  out = '/bic/zpl_pfila' )
                            ( in = 'ZPL_PGRAL'  out = '/bic/zpl_pgral' )
                            ( in = 'ZPL_QINPU'  out = '/bic/zpl_qinpu' )
                            ( in = 'ZPL_UPRIC'  out = '/bic/zpl_upric' )
                            ( in = 'ZQUANTITY'  out = '/bic/zquantity' )
                            ( in = '0COMP_CODE'   out = 'COMP_CODE' ) ).


    gt_fields_var    = VALUE #(
                                ( in = 'ZPL_DUM01'  out = '/bic/zpl_dum01' )
                                ( in = '0INFOPROV'      out = 'infoprov' )
                                ( in = '0USERNAME'      out = 'username' )
                                ( in = 'ZPLCOMPCO'      out = 'comp_code' )
                                ( in = '0FISCPER'      out = 'fiscper' )
                                ( in = '0FISCVARNT'     out = 'fiscvarnt' )
                                ( in = '0FISCYEAR'      out = 'fiscyear' )
                                ( in = '0CHRT_ACCTS'    out = 'chrt_accts' )
                                ( in = '0COORDER'       out = 'coorder' )
                                ( in = '0COSTCENTER'    out = 'costcenter' )
                                ( in = '0CO_AREA'       out = 'co_area' )
                                ( in = '0CURTYPE'       out = 'curtype ' )
                                ( in = '0GL_ACCOUNT'    out = 'gl_account' )
                                ( in = '0VERSION'       out = 'version' )
                                ( in = 'ZMAR'           out = '/bic/zmar' )
                                ( in = 'ZTABLA'         out = '/bic/ztabla' )
                                ( in = '0CURRENCY'      out = 'currency' )
                                ( in = '0UNIT'          out = 'unit' )
                                ( in = 'ZPL_COEF'   out = '/bic/zpl_coef' )
                                ( in = 'ZPL_COFIN'  out = '/bic/zpl_cofin' )
                                ( in = 'ZPL_PART'  out = '/bic/zpl_part' )
                                ( in = 'ZPL_VENTA'  out = '/bic/zpl_venta' )
                                ( in = 'ZPL_GASTP'  out = '/bic/zpl_gastp' )
                                ( in = 'ZPL_VENTP'  out = '/bic/zpl_ventp' )
                                ( in = 'ZPL_GASTO'  out = '/bic/zpl_gasto' )
                                ( in = 'ZQUANTITY'  out = '/bic/zquantity' )
                                ( in = '0COMP_CODE'   out = 'COMP_CODE' ) ).


    gt_fields_varq    = VALUE #(
                                ( in = 'ZPL_DUM01'  out = '/bic/zpl_dum01' )
                                ( in = '0INFOPROV'      out = 'infoprov' )
                                ( in = '0USERNAME'      out = 'username' )
                                    ( in = 'ZPLCOMPCO'      out = 'comp_code' )
                                   ( in = '0FISCPER'      out = 'fiscper' )
                                   ( in = '0FISCVARNT'     out = 'fiscvarnt' )
                                   ( in = '0FISCYEAR'      out = 'fiscyear' )
                                   ( in = '0CHRT_ACCTS'    out = 'chrt_accts' )
                                   ( in = '0COORDER'       out = 'coorder' )
                                   ( in = '0COSTCENTER'    out = 'costcenter' )
                                   ( in = '0CO_AREA'       out = 'co_area' )
                                   ( in = '0CURTYPE'       out = 'curtype ' )
                                   ( in = '0GL_ACCOUNT'    out = 'gl_account' )
                                   ( in = '0VERSION'       out = 'version' )
                                   ( in = 'ZTABLA'         out = '/bic/ztabla' )
                                   ( in = '0CURRENCY'      out = 'currency' )
                                   ( in = '0UNIT'          out = 'unit' )
                                   ( in = 'ZPL_VENTA'     out = '/bic/zpl_venta' )
                                   ( in = 'ZPL_VENTP'     out = '/bic/zpl_ventp' )
                                   ( in = 'ZPL_VTAQ'      out = '/bic/zpl_vtaq' )
                                   ( in = 'ZPL_VTAQP'     out = '/bic/zpl_vtaqp' )
                                   ( in = 'ZPL_GASTP'     out = '/bic/zpl_gastp' )
                                   ( in = 'ZPL_QINPU'  out = '/bic/zpl_pinpu' )
                                   ( in = 'ZPL_UPRIC'  out = '/bic/zpl_upric' )
                                   ( in = '0COMP_CODE'   out = 'COMP_CODE' ) ).


    gt_fields_abm     = VALUE #( ( in = 'COMP_CODE'   out = '0COMP_CODE' )
                               ( in = 'FISCVARNT'     out = '0FISCVARNT' )
                               ( in = 'FISCYEAR'      out = '0FISCYEAR' )
                               ( in = 'FISCPER'      out = '0FISCPER' )
                               ( in = 'CHRT_ACCTS'    out = '0CHRT_ACCTS' )
                               ( in = 'COORDER'       out = '0COORDER' )
                               ( in = 'COSTCENTER'    out = '0COSTCENTER' )
                               ( in = 'CO_AREA'       out = '0CO_AREA' )
                               ( in = 'GL_ACCOUNT'    out = '0GL_ACCOUNT' )
                               ( in = 'INFOPROV'      out = '0INFOPROV' )
                               ( in = 'USERNAME'      out = '0USERNAME' )
                               ( in = 'ZPL_DUM01'  out = 'ZPL_DUM01' ) ).


*ORDEN CUENTA  CEBE
*X       Y      Z

    gt_desicion = VALUE #(
 ( orden = 1 cuenta = 0 cebe = 0 resp = 'X' )
( orden = 1 cuenta = 0 cebe = 1 resp = 'X' )
( orden = 0 cuenta = 1 cebe = 0 resp = 'Y' )
( orden = 0 cuenta = 0 cebe = 1 resp = 'Z' )
( orden = 0 cuenta = 1 cebe = 1 resp = 'Y' )
( orden = 1 cuenta = 1 cebe = 0 resp = 'X' )
( orden = 1 cuenta = 1 cebe = 1 resp = 'X' )
( orden = 0 cuenta = 0 cebe = 0 resp = ' ' )
 ).


*PERIODOS Plan: Define el periodo de Salida de las combinaciones
    gt_period_plan = VALUE #(
*Periodo forecast
*       ( tipo = 'R' actual = '2025001' plan = '2025001')
*    ( tipo = 'R' actual = '2025002' plan = '2025002' )
*    ( tipo = 'R' actual = '2025003' plan = '2025003' )
*    ( tipo = 'R' actual = '2025004' plan = '2025004' )
*    ( tipo = 'R' actual = '2025005' plan = '2025005' )
*    ( tipo = 'R' actual = '2025006' plan = '2025006' )
*    ( tipo = 'R' actual = '2025007' plan = '2025007' )
*    ( tipo = 'R' actual = '2025008' plan = '2025008' )
*    ( tipo = 'R' actual = '2025009' plan = '2025009' )
*    ( tipo = 'R' actual = '2025010' plan = '2025010' )
*    ( tipo = 'R' actual = '2025011' plan = '2025011' )
*    ( tipo = 'R' actual = '2025012' plan = '2025012' )
*Periodo 26V0
    ( tipo = 'R' actual = '2025001' plan = '2026001')
    ( tipo = 'R' actual = '2025002' plan = '2026002' )
    ( tipo = 'R' actual = '2025003' plan = '2026003' )
    ( tipo = 'R' actual = '2025004' plan = '2026004' )
    ( tipo = 'R' actual = '2025005' plan = '2026005' )
    ( tipo = 'R' actual = '2025006' plan = '2026006' )
    ( tipo = 'R' actual = '2025007' plan = '2026007' )
    ( tipo = 'R' actual = '2025008' plan = '2026008' )
    ( tipo = 'R' actual = '2025009' plan = '2026009' )
    ( tipo = 'R' actual = '2025010' plan = '2026010' )
    ( tipo = 'R' actual = '2025011' plan = '2026011' )
    ( tipo = 'R' actual = '2025012' plan = '2026012' )
     ).



*------------------------------------------------
* PERIODOS Gastos (Plan/Actual) y Periodo Venta Ppto -  PXQ
    gt_period_pxq = VALUE #(
*Periodo para Forecast 25VF
*    ( tipo = 'R' actual = '2025001' plan = '2025001')
*    ( tipo = 'R' actual = '2025002' plan = '2025002' )
*    ( tipo = 'R' actual = '2025003' plan = '2025003' )
*    ( tipo = 'R' actual = '2025004' plan = '2025004' )
*    ( tipo = 'R' actual = '2025005' plan = '2025005' )
*    ( tipo = 'R' actual = '2025006' plan = '2025006' )
*    ( tipo = 'R' actual = '2025007' plan = '2025007' )
*    ( tipo = 'R' actual = '2025008' plan = '2025008' )
*    ( tipo = 'R' actual = '2025009' plan = '2025009' )
*    ( tipo = 'R' actual = '2025010' plan = '2025010' )
*    ( tipo = 'R' actual = '2025011' plan = '2025011' )
*    ( tipo = 'R' actual = '2025012' plan = '2025012' )
**PreCarga 26V0 2000:
*    ( tipo = 'R' actual = '2025001' plan = '2026001')
*    ( tipo = 'R' actual = '2025002' plan = '2026002' )
*    ( tipo = 'R' actual = '2025003' plan = '2026003' )
*    ( tipo = 'R' actual = '2025004' plan = '2026004' )
*    ( tipo = 'R' actual = '2025005' plan = '2026005' )
*    ( tipo = 'R' actual = '2025006' plan = '2026006' )
*    ( tipo = 'R' actual = '2025007' plan = '2026007' )
*    ( tipo = 'R' actual = '2025008' plan = '2026008' )
*    ( tipo = 'R' actual = '2025009' plan = '2026009' )
*    ( tipo = 'P' actual = '2025010' plan = '2026010' )
*    ( tipo = 'P' actual = '2025011' plan = '2026011' )
*    ( tipo = 'P' actual = '2025012'  plan = '2026012' )
**PreCarga 26V0 demas sociedades:
   ( tipo = 'R' actual = '2025001' plan = '2026001')
    ( tipo = 'R' actual = '2025002' plan = '2026002' )
    ( tipo = 'R' actual = '2025003' plan = '2026003' )
    ( tipo = 'R' actual = '2025004' plan = '2026004' )
    ( tipo = 'R' actual = '2025005' plan = '2026005' )
    ( tipo = 'R' actual = '2025006' plan = '2026006' )
    ( tipo = 'R' actual = '2025007' plan = '2026007' )
    ( tipo = 'R' actual = '2025008' plan = '2026008' )
    ( tipo = 'R' actual = '2025009' plan = '2026009' )
    ( tipo = 'R' actual = '2024010' plan = '2026010' )
    ( tipo = 'R' actual = '2024011' plan = '2026011' )
    ( tipo = 'R' actual = '2024012'  plan = '2026012' )

).



    DATA: lr_fiscper  LIKE LINE OF me->gr_fiscper_pxq,
          lr_fiscyear LIKE LINE OF me->gr_fiscyear.

    LOOP AT gt_period_pxq ASSIGNING FIELD-SYMBOL(<fs_period>).
      lr_fiscper-low = <fs_period>-actual.
      lr_fiscper-high = <fs_period>-tipo.
      lr_fiscper-option = 'EQ'.
      lr_fiscper-sign   = 'I'.
      APPEND lr_fiscper TO me->gr_fiscper_pxq.

      lr_fiscyear-low = <fs_period>-actual(4).

      lr_fiscyear-option = 'EQ'.
      lr_fiscyear-sign   = 'I'.
      APPEND lr_fiscyear TO me->gr_fiscyear.

    ENDLOOP.

    SORT me->gr_fiscyear.
    DELETE ADJACENT DUPLICATES FROM me->gr_fiscyear.
*-------------------------------------------------*
*PERIODOS Gastos Plan/Actual VAR
    gt_period_var = VALUE #(
*   ( tipo = 'R' actual = '2025001' plan = '2025001')
*    ( tipo = 'R' actual = '2025002' plan = '2025002' )
*    ( tipo = 'R' actual = '2025003' plan = '2025003' )
*    ( tipo = 'R' actual = '2025004' plan = '2025004' )
*    ( tipo = 'R' actual = '2025005' plan = '2025005' )
*    ( tipo = 'R' actual = '2025006' plan = '2025006' )
*    ( tipo = 'R' actual = '2025007' plan = '2025007' )
*    ( tipo = 'R' actual = '2025008' plan = '2025008' )
*    ( tipo = 'R' actual = '2025009' plan = '2025009' )
*    ( tipo = 'R' actual = '2025010' plan = '2025010' )
*    ( tipo = 'R' actual = '2025011' plan = '2025011' )
*    ( tipo = 'R' actual = '2025012' plan = '2025012' )
*

*PreCarga 26V0 2000:
*    ( tipo = 'R' actual = '2025001' plan = '2026001')
*    ( tipo = 'R' actual = '2025002' plan = '2026002' )
*    ( tipo = 'R' actual = '2025003' plan = '2026003' )
*    ( tipo = 'R' actual = '2025004' plan = '2026004' )
*    ( tipo = 'R' actual = '2025005' plan = '2026005' )
*    ( tipo = 'R' actual = '2025006' plan = '2026006' )
*    ( tipo = 'R' actual = '2025007' plan = '2026007' )
*    ( tipo = 'R' actual = '2025008' plan = '2026008' )
*    ( tipo = 'R' actual = '2025009' plan = '2026009' )
*    ( tipo = 'P' actual = '2025010' plan = '2026010' )
*    ( tipo = 'P' actual = '2025011' plan = '2026011' )
*    ( tipo = 'P' actual = '2025012'  plan = '2026012' )
**PreCarga 26V0 demas sociedades:
   ( tipo = 'R' actual = '2025001' plan = '2026001')
    ( tipo = 'R' actual = '2025002' plan = '2026002' )
    ( tipo = 'R' actual = '2025003' plan = '2026003' )
    ( tipo = 'R' actual = '2025004' plan = '2026004' )
    ( tipo = 'R' actual = '2025005' plan = '2026005' )
    ( tipo = 'R' actual = '2025006' plan = '2026006' )
    ( tipo = 'R' actual = '2025007' plan = '2026007' )
    ( tipo = 'R' actual = '2025008' plan = '2026008' )
    ( tipo = 'R' actual = '2025009' plan = '2026009' )
    ( tipo = 'R' actual = '2024010' plan = '2026010' )
    ( tipo = 'R' actual = '2024011' plan = '2026011' )
    ( tipo = 'R' actual = '2024012'  plan = '2026012' )

     ).




    LOOP AT gt_period_var ASSIGNING <fs_period>.

      lr_fiscper-low = <fs_period>-actual.
      lr_fiscper-high = <fs_period>-tipo.
      lr_fiscper-option = 'EQ'.
      lr_fiscper-sign   = 'I'.
      APPEND lr_fiscper TO me->gr_fiscper_var.

    ENDLOOP.

    gt_period_var_2000 = VALUE #(
*   ( tipo = 'R' actual = '2025001' plan = '2025001')
*    ( tipo = 'R' actual = '2025002' plan = '2025002' )
*    ( tipo = 'R' actual = '2025003' plan = '2025003' )
*    ( tipo = 'R' actual = '2025004' plan = '2025004' )
*    ( tipo = 'R' actual = '2025005' plan = '2025005' )
*    ( tipo = 'R' actual = '2025006' plan = '2025006' )
*    ( tipo = 'R' actual = '2025007' plan = '2025007' )
*    ( tipo = 'R' actual = '2025008' plan = '2025008' )
*    ( tipo = 'R' actual = '2025009' plan = '2025009' )
*    ( tipo = 'R' actual = '2025010' plan = '2025010' )
*    ( tipo = 'R' actual = '2025011' plan = '2025011' )
*    ( tipo = 'R' actual = '2025012' plan = '2025012' )
*

*PreCarga 26V0 2000:
       ( tipo = 'R' actual = '2025001' plan = '2026001')
       ( tipo = 'R' actual = '2025002' plan = '2026002' )
       ( tipo = 'R' actual = '2025003' plan = '2026003' )
       ( tipo = 'R' actual = '2025004' plan = '2026004' )
       ( tipo = 'R' actual = '2025005' plan = '2026005' )
       ( tipo = 'R' actual = '2025006' plan = '2026006' )
       ( tipo = 'R' actual = '2025007' plan = '2026007' )
       ( tipo = 'R' actual = '2025008' plan = '2026008' )
       ( tipo = 'R' actual = '2025009' plan = '2026009' )
       ( tipo = 'P' actual = '2025010' plan = '2026010' )
       ( tipo = 'P' actual = '2025011' plan = '2026011' )
       ( tipo = 'P' actual = '2025012'  plan = '2026012' )

        ).




    LOOP AT gt_period_var_2000 ASSIGNING <fs_period>.

      lr_fiscper-low = <fs_period>-actual.
      lr_fiscper-high = <fs_period>-tipo.
      lr_fiscper-option = 'EQ'.
      lr_fiscper-sign   = 'I'.
      APPEND lr_fiscper TO me->gr_fiscper_var_2000.

    ENDLOOP.


*-------------------------------------------------*
*PERIODOS para Gastos: VARQ

    gt_period_varq = VALUE #(
*   ( tipo = 'R' actual = '2025001' plan = '2025001')
*    ( tipo = 'R' actual = '2025002' plan = '2025002' )
*    ( tipo = 'R' actual = '2025003' plan = '2025003' )
*    ( tipo = 'R' actual = '2025004' plan = '2025004' )
*    ( tipo = 'R' actual = '2025005' plan = '2025005' )
*    ( tipo = 'R' actual = '2025006' plan = '2025006' )
*    ( tipo = 'R' actual = '2025007' plan = '2025007' )
*    ( tipo = 'R' actual = '2025008' plan = '2025008' )
*    ( tipo = 'R' actual = '2025009' plan = '2025009' )
*    ( tipo = 'R' actual = '2025010' plan = '2025010' )
*    ( tipo = 'R' actual = '2025011' plan = '2025011' )
*    ( tipo = 'R' actual = '2025012' plan = '2025012' )
**

**PreCarga 26V0 demas sociedades:
   ( tipo = 'R' actual = '2025001' plan = '2026001')
    ( tipo = 'R' actual = '2025002' plan = '2026002' )
    ( tipo = 'R' actual = '2025003' plan = '2026003' )
    ( tipo = 'R' actual = '2025004' plan = '2026004' )
    ( tipo = 'R' actual = '2025005' plan = '2026005' )
    ( tipo = 'R' actual = '2025006' plan = '2026006' )
    ( tipo = 'R' actual = '2025007' plan = '2026007' )
    ( tipo = 'R' actual = '2025008' plan = '2026008' )
    ( tipo = 'R' actual = '2025009' plan = '2026009' )
    ( tipo = 'R' actual = '2024010' plan = '2026010' )
    ( tipo = 'R' actual = '2024011' plan = '2026011' )
    ( tipo = 'R' actual = '2024012'  plan = '2026012' )

     ).




    LOOP AT gt_period_varq ASSIGNING <fs_period>.

      lr_fiscper-low = <fs_period>-plan.

      lr_fiscper-option = 'EQ'.
      lr_fiscper-sign   = 'I'.
      APPEND lr_fiscper TO me->gr_fiscper_var.

    ENDLOOP.

    gt_period_var_2000 = VALUE #(
*   ( tipo = 'R' actual = '2025001' plan = '2025001')
*    ( tipo = 'R' actual = '2025002' plan = '2025002' )
*    ( tipo = 'R' actual = '2025003' plan = '2025003' )
*    ( tipo = 'R' actual = '2025004' plan = '2025004' )
*    ( tipo = 'R' actual = '2025005' plan = '2025005' )
*    ( tipo = 'R' actual = '2025006' plan = '2025006' )
*    ( tipo = 'R' actual = '2025007' plan = '2025007' )
*    ( tipo = 'R' actual = '2025008' plan = '2025008' )
*    ( tipo = 'R' actual = '2025009' plan = '2025009' )
*    ( tipo = 'R' actual = '2025010' plan = '2025010' )
*    ( tipo = 'R' actual = '2025011' plan = '2025011' )
*    ( tipo = 'R' actual = '2025012' plan = '2025012' )
**

*PreCarga 26V0 2000:
       ( tipo = 'R' actual = '2025001' plan = '2026001')
       ( tipo = 'R' actual = '2025002' plan = '2026002' )
       ( tipo = 'R' actual = '2025003' plan = '2026003' )
       ( tipo = 'R' actual = '2025004' plan = '2026004' )
       ( tipo = 'R' actual = '2025005' plan = '2026005' )
       ( tipo = 'R' actual = '2025006' plan = '2026006' )
       ( tipo = 'R' actual = '2025007' plan = '2026007' )
       ( tipo = 'R' actual = '2025008' plan = '2026008' )
       ( tipo = 'R' actual = '2025009' plan = '2026009' )
       ( tipo = 'P' actual = '2025010' plan = '2026010' )
       ( tipo = 'P' actual = '2025011' plan = '2026011' )
       ( tipo = 'P' actual = '2025012'  plan = '2026012' )

        ).



    LOOP AT gt_period_varq_2000 ASSIGNING <fs_period>.

      lr_fiscper-low = <fs_period>-plan.

      lr_fiscper-option = 'EQ'.
      lr_fiscper-sign   = 'I'.
      APPEND lr_fiscper TO me->gr_fiscper_varq_2000.

    ENDLOOP.
*-------------------------------------------------*
*PERIODOS Para Venta Actual/Real

    gt_period_vta = VALUE #(
    ( tipo = 'R' actual = '2025001' plan = '2025001')
    ( tipo = 'R' actual = '2025002' plan = '2025002' )
    ( tipo = 'R' actual = '2025003' plan = '2025003' )
    ( tipo = 'R' actual = '2025004' plan = '2025004' )
    ( tipo = 'R' actual = '2025005' plan = '2025005' )
    ( tipo = 'R' actual = '2025006' plan = '2025006' )
    ( tipo = 'R' actual = '2025007' plan = '2025007' )
    ( tipo = 'R' actual = '2025008' plan = '2025008' )
    ( tipo = 'R' actual = '2025009' plan = '2025009' )
    ( tipo = 'R' actual = '2025010' plan = '2025010' )
    ( tipo = 'R' actual = '2025011' plan = '2025011' )
    ( tipo = 'R' actual = '2025012' plan = '2025012' )
     ).

*-------------------------------------------------*
    DATA: wa_zvar LIKE LINE OF gr_zvar.

    wa_zvar-sign = 'I'.
    wa_zvar-option = 'EQ'.
    wa_zvar-low = 'VAE'.
    APPEND wa_zvar TO gr_zvar.
    wa_zvar-low = 'SEM'.
    APPEND wa_zvar TO gr_zvar.

    DATA: wa_zpxq LIKE LINE OF gr_zpxq.

    wa_zpxq-sign = 'I'.
    wa_zpxq-option = 'EQ'.
    wa_zpxq-low = 'DIS'.
    APPEND wa_zpxq TO gr_zpxq.
    wa_zpxq-low = 'FLE'.
    APPEND wa_zpxq TO gr_zpxq.
    wa_zpxq-low = 'REC'.
    APPEND wa_zpxq TO gr_zpxq.
    wa_zpxq-low = 'INV'.
    APPEND wa_zpxq TO gr_zpxq.
    wa_zpxq-low = 'INE'.
    APPEND wa_zpxq TO gr_zpxq.
    wa_zpxq-low = 'HOM'.
    APPEND wa_zpxq TO gr_zpxq.

    DATA: wa_zvarq LIKE LINE OF gr_zvarq.

    wa_zvarq-sign = 'I'.
    wa_zvarq-option = 'EQ'.
    wa_zvarq-low = 'VAC'.
    APPEND wa_zvarq TO gr_zvarq.

*-------------------------------------------------*
    DATA: wa_user LIKE LINE OF gr_user_pxq.

    wa_user-sign = 'I'.
    wa_user-option = 'EQ'.
    APPEND wa_user TO gr_user_pxq.
    wa_user-low = 'ERNGUERR'.
    APPEND wa_user TO gr_user_pxq.
    wa_user-low = 'ERIPEREZ'.
    APPEND wa_user TO gr_user_pxq.
    wa_user-low = 'LAUGARAY'.
    APPEND wa_user TO gr_user_pxq.
    wa_user-low = 'TANIDIAZ'.
    APPEND wa_user TO gr_user_pxq.
    wa_user-low = 'ELEREYES'.
    APPEND wa_user TO gr_user_pxq.
    wa_user-low = 'FABRIVAS'.
    APPEND wa_user TO gr_user_pxq.
    wa_user-low = 'VALVELAZ'.
    APPEND wa_user TO gr_user_pxq.
    wa_user-low = 'MARENCIN'.
    APPEND wa_user TO gr_user_pxq.



    SELECT /bic/zpl_ibmus AS user_ibm,
           username AS user_sap
    INTO TABLE @lt_users
* ACA CAMBIAR DE 28 A 42 PARA FORECAST
    FROM /bic/azpl_a287.
*    WHERE /bic/zpl_dum01 = 1. 23/1/2025 notamos que no se estaba llenando la tabla debido a que no es necesario ver el status en este constructor.

    IF sy-subrc = 0.

      APPEND LINES OF lt_users TO gt_user.
      DELETE ADJACENT DUPLICATES FROM gt_user.

    ENDIF.



* Carga Cuentas excepción:

    SELECT 'I' AS sign,
            'EQ' AS option,
            a~gl_account
    INTO TABLE @me->gt_cuentas
    FROM /bic/azpl_a292 AS a
    WHERE a~/bic/zpl_dum01 EQ 1.
    IF sy-subrc EQ 0.


    ENDIF.



  ENDMETHOD.


  METHOD derive_user.


*  IMPORTING i_soc    TYPE char4
*                              i_soco   TYPE char4
*                              i_cuenta TYPE char10
*                              i_cebe   TYPE char10
*                              i_orden  TYPE char12
*                    EXPORTING o_user   TYPE char12
*                              o_result TYPE  rs_bool
*                              o_msg    TYPE if_rspls_cr_types=>tn_s_mesg,
*g

    DATA: wa_clave   TYPE ty_wa_desicion,
          l_ibm_user TYPE char3.



*1) Busca los Técnicos correspondientes:



*1.1) ORDEN
    SELECT SINGLE a~resp_user
     INTO @DATA(l_user_orden)
     FROM /bi0/mcoorder AS a
     WHERE a~coorder EQ @i_orden
     AND a~objvers EQ 'A'.

    IF sy-subrc EQ 0 AND l_user_orden IS NOT INITIAL.
      wa_clave-orden = 1.
    ELSE.
      wa_clave-orden = 0.
    ENDIF.


    DATA: l_user_cebe TYPE /bic/oiztecnico,
          l_zabc      TYPE /bic/oizabc.
*1.2) CEBE
    SELECT SINGLE a~/bic/ztecnico, a~/bic/zabc
          INTO (  @l_user_cebe,  @l_zabc )
          FROM /bi0/mprofit_ctr AS a
          WHERE a~profit_ctr EQ @i_cebe
          AND a~co_area EQ @i_soco
          AND a~objvers EQ 'A'
          AND a~dateto EQ '99991231'.

    IF sy-subrc EQ 0 AND l_user_cebe IS NOT INITIAL.
      wa_clave-cebe = 1.
    ELSE.
      wa_clave-cebe = 0.
    ENDIF.




*1.3) CUENTA
    SELECT SINGLE
             a~/bic/ztec_cuen,
             a~/bic/ztec_cue2,
             a~/bic/ztec_cue3,
             a~/bic/ztec_cue4,
             a~/bic/ztec_cue5,
             a~/bic/ztec_cue6
             INTO @DATA(wa_cuenta)
             FROM /bi0/mgl_account AS a
             WHERE a~gl_account EQ @i_cuenta
             AND a~chrt_accts EQ 'SQS'
             AND a~objvers EQ 'A'.
    IF sy-subrc EQ 0.
      CASE i_soc.
        WHEN '1000'.
          IF l_zabc EQ 'FABRICA'.
            IF   wa_cuenta-/bic/ztec_cue2 IS NOT INITIAL.
              wa_clave-cuenta = 1.
            ELSE.
              wa_clave-cuenta = 0.
            ENDIF.
          ELSE.
            IF   wa_cuenta-/bic/ztec_cuen IS NOT INITIAL.
              wa_clave-cuenta = 1.
            ELSE.
              wa_clave-cuenta = 0.
            ENDIF.
          ENDIF.
        WHEN '2000'.
          IF wa_cuenta-/bic/ztec_cue4 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.

        WHEN '3000'.
          IF wa_cuenta-/bic/ztec_cue3 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.

        WHEN '6000'.
          IF wa_cuenta-/bic/ztec_cue6 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.


        WHEN '7000'.
          IF wa_cuenta-/bic/ztec_cue5 IS NOT INITIAL.
            wa_clave-cuenta = 1.
          ELSE.
            wa_clave-cuenta = 0.
          ENDIF.

      ENDCASE.

    ENDIF.






*3) Busca en la matriz de desición, cual es el Responsable a asignar
    READ TABLE gt_desicion ASSIGNING FIELD-SYMBOL(<fs_des>)
    WITH KEY orden = wa_clave-orden
             cebe = wa_clave-cebe
             cuenta = wa_clave-cuenta.

    IF sy-subrc EQ 0.

      wa_clave-resp = <fs_des>-resp.

      CASE wa_clave-resp.

        WHEN 'Y'."Valida Técnico CUENTA

          CASE i_soc.
            WHEN '1000'.
              IF wa_cuenta-/bic/ztec_cue2 IS NOT INITIAL AND l_zabc EQ 'FABRICA'.
                l_ibm_user = wa_cuenta-/bic/ztec_cue2.
                o_result = 'X'.
              ELSEIF wa_cuenta-/bic/ztec_cuen IS NOT INITIAL.
                l_ibm_user = wa_cuenta-/bic/ztec_cuen.
                o_result = 'X'.
              ELSE.

                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '1000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'No se encontro Tecnico Cuenta' ` ` INTO o_msg-msgv1..
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.

                o_result = ' '.
              ENDIF.
            WHEN '2000'.
              IF wa_cuenta-/bic/ztec_cue4 IS NOT INITIAL.
                l_ibm_user = wa_cuenta-/bic/ztec_cue4.
                o_result = 'X'.
              ELSE.

                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '2000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'No se encontro Tecnico Cuenta' ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.
              ENDIF.

            WHEN '3000'.
              IF wa_cuenta-/bic/ztec_cue3 IS NOT INITIAL.
                l_ibm_user = wa_cuenta-/bic/ztec_cue3.
                o_result = 'X'.
              ELSE.
                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '3000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'No se encontro Tecnico Cuenta' ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.

                o_result = ' '.
              ENDIF.

            WHEN '6000'.
              IF wa_cuenta-/bic/ztec_cue6 IS NOT INITIAL.
                l_ibm_user = wa_cuenta-/bic/ztec_cue6.
                o_result = 'X'.
              ELSE.
                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '6000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'No se encontro Tecnico Cuenta' ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.
                o_result = ' '.
              ENDIF.


            WHEN '7000'.
              IF wa_cuenta-/bic/ztec_cue5 IS NOT INITIAL.
                l_ibm_user = wa_cuenta-/bic/ztec_cue5.
                o_result = 'X'.
              ELSE.
                o_msg-msgid = 'CUENTA'.
                o_msg-msgno = '7000'.
                o_msg-msgty = 'E'.
                CONCATENATE 'No se encontro Tecnico Cuenta' ` ` INTO o_msg-msgv1.
                o_msg-msgv2 = i_cuenta.
                o_msg-msgv3 = i_cebe.
                o_msg-msgv4 = i_orden.
                o_result = ' '.
                o_result = ' '.
              ENDIF.

          ENDCASE.




        WHEN 'Z'."Valida Técnico CEBE
          IF l_user_cebe IS NOT INITIAL.
            l_ibm_user = l_user_cebe.
            o_result = 'X'.
*            completar mensaje
          ELSE.
            o_result = ' '.
            o_msg-msgid = 'CEBE'.
            o_msg-msgno = i_soc.
            o_msg-msgty = 'E'.
            CONCATENATE 'No se encontro Tecnico CEBE:' ` `  INTO o_msg-msgv1.
            o_msg-msgv2 = i_cuenta.
            o_msg-msgv3 = i_cebe.
            o_msg-msgv4 = i_orden.

*            completar mensaje
          ENDIF.


        WHEN 'X'."Valida Técnico Orden
          IF l_user_orden IS NOT INITIAL.
            l_ibm_user = l_user_orden.
            o_result = 'X'.
*            completar mensaje
          ELSE.
            o_msg-msgid = 'ORDEN'.
            o_msg-msgno = i_soc.
            o_msg-msgty = 'E'.
            CONCATENATE 'No se encontro Tecnico Orden' ' ' INTO o_msg-msgv1.
            o_msg-msgv2 = i_cuenta.
            o_msg-msgv3 = i_cebe.
            o_msg-msgv4 = i_orden.

            o_result = ' '.
*            completar mensaje
          ENDIF.

      ENDCASE.
    ELSE.
      "Clave no encontrada: mensaje

    ENDIF.


*4)  Busca el usuario IBM, segun el usuario SAP ingresado
    IF l_ibm_user IS INITIAL.
*     Llenar mensaje de respuesta.
      o_result = ' '.
      EXIT.
    ENDIF.

    me->get_sap_user(
      EXPORTING
        i_ibm_user = l_ibm_user
 IMPORTING
        o_sap_user = o_user
    ).




  ENDMETHOD.


  METHOD get_actual_period.

    DATA: l_anio TYPE num4.

    CASE i_tipo.

      WHEN 'PXQ'.

        READ TABLE gt_period_pxq INTO DATA(wa_period) WITH KEY  actual = i_period.
        IF sy-subrc EQ 0.
          IF me->g_tipo_actual EQ 'P'.
            o_actual = wa_period-plan.
          ELSEIF me->g_tipo_actual EQ 'R'.
            o_actual = wa_period-actual.
          ENDIF.
        ENDIF.

      WHEN 'VAR'.
        READ TABLE gt_period_var INTO wa_period WITH KEY  actual = i_period.
        IF me->g_tipo_actual EQ 'P'.
          o_actual = wa_period-plan.
        ELSEIF me->g_tipo_actual EQ 'R'.
          o_actual = wa_period-actual.
        ENDIF.

      WHEN 'VARQ'.
        READ TABLE gt_period_varq INTO wa_period WITH KEY  actual = i_period.
        IF me->g_tipo_actual EQ 'P'.
          o_actual = wa_period-plan.
        ELSEIF me->g_tipo_actual EQ 'R'.
          o_actual = wa_period-actual.
        ENDIF.


      WHEN 'VTA'.
        READ TABLE gt_period_vta INTO wa_period WITH KEY  plan = i_period.
        IF me->g_tipo_actual EQ 'P'.
          o_actual = wa_period-plan.
        ELSEIF me->g_tipo_actual EQ 'R'.
          o_actual = wa_period-actual.
        ENDIF.



    ENDCASE.


  ENDMETHOD.


  METHOD get_claves.
*
* get_claves IMPORTING   i_soc    TYPE char4
*                             i_soco   TYPE char4
*                             i_cuenta TYPE char10
*                             i_cebe   TYPE char10
*                             i_orden  TYPE char12
*                             i_user   TYPE char12
*                   EXPORTING o_claves type ty_lt_claves
*                             o_result TYPE  rs_bool
*                             o_msg    TYPE if_rspls_cr_types=>tn_s_mesg,




    DATA: l_user_cebe TYPE /bic/oiztecnico,
          l_zabc      TYPE /bic/oizabc,
          lt_linea    TYPE rsr_t_rangesid,
          lt_ofvta    TYPE rsr_t_rangesid,
          lt_canal    TYPE rsr_t_rangesid,
          lr_linea    TYPE RANGE OF /bi0/oimatl_group,
          lr_ofvta    TYPE RANGE OF /bi0/oisales_off,
          lr_canal    TYPE RANGE OF /bi0/oidistr_chan,
          lt_auths    TYPE rsec_tsx_auths,
          lt_leaves   TYPE rrsv_t_sid.





    me->get_ibm_user(
      EXPORTING
        i_sap_user = i_user
      IMPORTING
        o_ibm_user = DATA(l_ibm_user)

    ).

*    IF i_user NE me->g_users-user_sap.

    me->g_users-user_sap = i_user.
    me->g_users-user_ibm =  l_ibm_user.
*1)  Obtiene todos las Ordenes del Usuario y aquellas que no tienen Tecnico


*1.1) ORDEN
*      IF me->lt_orden[] IS INITIAL or
    IF i_orden[] NE gr_orden[] OR me->lt_orden[] IS INITIAL.
      gr_orden[] = i_orden[].
*        ENDIF.

      SELECT a~resp_user, a~coorder
       INTO TABLE @DATA(lt_ordenes)
       FROM zpl_coorder AS a
       WHERE a~coorder IN @gr_orden
       AND ( a~resp_user EQ @l_ibm_user OR a~resp_user EQ '').
      IF sy-subrc EQ 0.
        SORT lt_ordenes BY resp_user DESCENDING coorder.
      ENDIF.
      me->lt_orden = CORRESPONDING ty_lt_claves( lt_ordenes MAPPING username = resp_user ).
    ENDIF.





*1.2) CEBE
*      IF me->lt_cebe[] IS INITIAL.
    IF i_cebe[] NE gr_cebe[] OR me->lt_cebe[] IS INITIAL.
      gr_cebe[] = i_cebe[].
*        ENDIF.

*   Busca Lineas autorizadas
      CALL FUNCTION 'RSEC_GET_AUTH_FOR_USER'
        EXPORTING
          i_iobjnm                    = '0PROFIT_CTR__0MATL_GROUP'
*         i_infocube                  =
          i_uname                     = i_user
*         i_no_warnings               = rs_c_false
*         i_ignore_hierarchies        = rs_c_false
*         i_separate_leaves           = rs_c_false
          i_complete_answer           = rs_c_true
*         i_r_olap_area               =
        IMPORTING
          e_t_rangesid                = lt_linea[]
          e_tsx_auths                 = lt_auths
          e_t_leaves_sids             = lt_leaves
        EXCEPTIONS
          not_authorized              = 1
          wrong_parameter_combination = 2
          internal_error              = 3
          iobj_not_found              = 4
          x_message                   = 5
          OTHERS                      = 6.
      IF sy-subrc EQ 0.
        lr_linea = CORRESPONDING #( lt_linea MAPPING option = opt ).
      ELSE.
        CLEAR lr_linea.
      ENDIF.

*   Busca Oficina de Ventas autorizadas
      CALL FUNCTION 'RSEC_GET_AUTH_FOR_USER'
        EXPORTING
          i_iobjnm                    = '0PROFIT_CTR__0SALES_OFF'
*         i_infocube                  =
          i_uname                     = i_user
*         i_no_warnings               = rs_c_false
*         i_ignore_hierarchies        = rs_c_false
*         i_separate_leaves           = rs_c_false
          i_complete_answer           = rs_c_true
*         i_r_olap_area               =
        IMPORTING
          e_t_rangesid                = lt_ofvta[]
          e_tsx_auths                 = lt_auths
          e_t_leaves_sids             = lt_leaves
        EXCEPTIONS
          not_authorized              = 1
          wrong_parameter_combination = 2
          internal_error              = 3
          iobj_not_found              = 4
          x_message                   = 5
          OTHERS                      = 6.
      IF sy-subrc EQ 0.
        lr_ofvta = CORRESPONDING #( lt_ofvta MAPPING option = opt ).
      ELSE.
        CLEAR lr_ofvta.
      ENDIF.



*   Busca Oficina de Ventas autorizadas
      CALL FUNCTION 'RSEC_GET_AUTH_FOR_USER'
        EXPORTING
          i_iobjnm                    = '0PROFIT_CTR__0DISTR_CHAN'
*         i_infocube                  =
          i_uname                     = i_user
*         i_no_warnings               = rs_c_false
*         i_ignore_hierarchies        = rs_c_false
*         i_separate_leaves           = rs_c_false
          i_complete_answer           = rs_c_true
*         i_r_olap_area               =
        IMPORTING
          e_t_rangesid                = lt_canal[]
          e_tsx_auths                 = lt_auths
          e_t_leaves_sids             = lt_leaves
        EXCEPTIONS
          not_authorized              = 1
          wrong_parameter_combination = 2
          internal_error              = 3
          iobj_not_found              = 4
          x_message                   = 5
          OTHERS                      = 6.
      IF sy-subrc EQ 0.
        lr_canal = CORRESPONDING #( lt_canal MAPPING option = opt ).
      ELSE.
        CLEAR lr_canal.
      ENDIF.





      SELECT a~/bic/ztecnico, a~profit_ctr, a~/bic/zabc
            INTO TABLE @DATA(lt_cebe)
            FROM /bi0/mprofit_ctr AS a
            WHERE ( a~/bic/ztecnico EQ @l_ibm_user OR a~/bic/ztecnico EQ '')
            AND a~co_area EQ @i_soco
            AND a~profit_ctr IN @gr_cebe
            AND a~matl_group IN @lr_linea
            AND a~sales_off  IN @lr_ofvta
            AND a~distr_chan IN @lr_canal
            AND a~objvers EQ 'A'
            AND a~dateto EQ '99991231'.

      IF sy-subrc EQ 0.
        SORT lt_cebe BY profit_ctr /bic/ztecnico DESCENDING   .
      ENDIF.
      me->lt_cebe = CORRESPONDING ty_lt_claves( lt_cebe MAPPING username = /bic/ztecnico ).

    ENDIF.


*1.3) CUENTA
*      IF me->lt_cuenta[] IS INITIAL.
    IF i_cuenta[] NE gr_cuenta[] OR me->lt_cuenta[] IS INITIAL.
      gr_cuenta = i_cuenta[].
*        ENDIF.



      CASE i_soc.
        WHEN '1000'.
          SELECT a~/bic/ztec_cuen, a~/bic/ztec_cue2, a~gl_account
*             a~/bic/ztec_cue3,
*             a~/bic/ztec_cue4,
*             a~/bic/ztec_cue5,
*             a~/bic/ztec_cue6
            INTO TABLE @DATA(lt_cuenta)
            FROM /bi0/mgl_account AS a
            WHERE ( a~/bic/ztec_cuen EQ @l_ibm_user OR a~/bic/ztec_cuen EQ '' )
            AND a~chrt_accts EQ 'SQS'
            AND a~gl_account IN @gr_cuenta
            AND a~/bic/znivel1 EQ '07'
            AND ( a~/bic/zcla_cue EQ 'P' OR a~/bic/zcla_cue EQ 'S' )
            AND a~objvers EQ 'A'.
          IF sy-subrc EQ 0.
            SORT lt_cuenta BY  /bic/ztec_cuen DESCENDING.
          ENDIF.
*      WHEN '2000'.
*        IF wa_cuenta-/bic/ztec_cue4 IS NOT INITIAL.
*          wa_clave-cuenta = 1.
*        ELSE.
*          wa_clave-cuenta = 0.
*        ENDIF.
*
*      WHEN '3000'.
*        IF wa_cuenta-/bic/ztec_cue3 IS NOT INITIAL.
*          wa_clave-cuenta = 1.
*        ELSE.
*          wa_clave-cuenta = 0.
*        ENDIF.
*
*      WHEN '6000'.
*        IF wa_cuenta-/bic/ztec_cue6 IS NOT INITIAL.
*          wa_clave-cuenta = 1.
*        ELSE.
*          wa_clave-cuenta = 0.
*        ENDIF.
*
*
*      WHEN '7000'.
*        IF wa_cuenta-/bic/ztec_cue5 IS NOT INITIAL.
*          wa_clave-cuenta = 1.
*        ELSE.
*          wa_clave-cuenta = 0.
*        ENDIF.

      ENDCASE.
      me->lt_cuenta = CORRESPONDING ty_lt_claves( lt_cuenta MAPPING username = /bic/ztec_cuen ).
    ENDIF.

*    ELSE.


*      o_claves[] = me->gt_claves[].

    DATA: lt_aux TYPE ty_lt_claves_aux.

*   Si ya se hizo la carga inicial, se pasa solo aquellos registros que no fueron definidos:
    IF me->lt_orden[] IS NOT INITIAL.
      APPEND LINES OF me->lt_orden TO lt_aux.
    ENDIF.


    IF me->lt_cuenta[] IS NOT INITIAL.
      APPEND LINES OF me->lt_cuenta TO lt_aux.
    ENDIF.

    IF me->lt_cebe[]  IS NOT INITIAL.
      APPEND LINES OF me->lt_cebe TO lt_aux.
    ENDIF.

    o_claves[] = lt_aux[].

*    ENDIF.


  ENDMETHOD.


  METHOD get_forecast_table.
    o_table = c_forecast.
  ENDMETHOD.


  METHOD get_ibm_user.
*  importing i_sap_user type char12
*                   exporting o_ibm_user type char3,

    READ TABLE gt_user  ASSIGNING FIELD-SYMBOL(<fs_user>)
    WITH KEY sap_key COMPONENTS user_sap = i_sap_user.
    IF sy-subrc EQ 0.
      o_ibm_user = <fs_user>-user_ibm.
    ENDIF.



  ENDMETHOD.


  METHOD get_locked_entries.
    DATA: l_s_mesg  TYPE if_rspls_cr_types=>tn_s_mesg,
          lv_status TYPE /bic/oizpl_dum01.

    FIELD-SYMBOLS:
      <fs_compcode> TYPE /bi0/oicomp_code,
      <fs_user>     TYPE /bi0/oiusername.




    ASSIGN COMPONENT:
    'COMP_CODE' OF STRUCTURE i_s_data TO <fs_compcode>,
    'USERNAME' OF STRUCTURE i_s_data TO <fs_user>.


    CHECK sy-subrc EQ 0 AND me->c_unblock_act IS INITIAL."Si se flaguea para actualizacion Vta Presup, no valida Bloqueo


*Busca excecpiones:

    CLEAR lv_status.

    SELECT SINGLE  /bic/zpl_dum01
    FROM /bic/azpl_a287 AS a
    INTO lv_status
    WHERE /bic/zplcompco = <fs_compcode>
    AND username = <fs_user>
    AND /bic/zpl_link = i_link.



    IF sy-subrc EQ 0.
      IF lv_status EQ 0.
        e_s_mesg-msgid = 'ZPL'.
        e_s_mesg-msgno = '000'. "Usuario está bloqueado.
        e_s_mesg-msgty = 'W'.
        e_s_mesg-msgv1 = <fs_compcode>.
        e_s_mesg-msgv2 = <fs_user>.
        e_s_mesg-msgv3 = i_link.

        e_noinput = rs_c_true.
      ELSEIF lv_status EQ 1.
        e_s_mesg-msgid = 'ZPL'.
        e_s_mesg-msgno = '003'. "Usuario está habilitado.
        e_s_mesg-msgty = 'S'.
        e_s_mesg-msgv1 = <fs_compcode>.
        e_s_mesg-msgv2 = <fs_user>.
        e_s_mesg-msgv3 = i_link.

        e_noinput = rs_c_false.
      ENDIF.

    ELSE.
* Por defecto el usuario se bloquea:
      IF me->c_block_default = 'X'.
        e_s_mesg-msgid = 'ZPL'.
        e_s_mesg-msgno = '000'. "Usuario está bloqueado.
        e_s_mesg-msgty = 'W'.
        e_s_mesg-msgv1 = <fs_compcode>.
        e_s_mesg-msgv2 = <fs_user>.
        e_s_mesg-msgv3 = i_link.
        e_noinput = rs_c_true.
      ENDIF.

    ENDIF.



  ENDMETHOD.


  METHOD get_plan_period.
    DATA: l_anio TYPE num4.

    CASE i_tipo.

      WHEN 'PXQ'.

        READ TABLE gt_period_pxq INTO DATA(wa_period) WITH KEY  actual = i_period.
        IF sy-subrc EQ 0.
          o_plan = wa_period-plan.

        ENDIF.
      WHEN 'VAR'.
        READ TABLE gt_period_var INTO wa_period WITH KEY  actual = i_period.
        IF sy-subrc EQ 0.
          l_anio = wa_period-plan(4).
          CONCATENATE l_anio wa_period-plan+4(3) INTO o_plan.
        ENDIF.

      WHEN 'VARQ'.
        READ TABLE gt_period_varq INTO wa_period WITH KEY  actual = i_period.
        IF sy-subrc EQ 0.

          l_anio = wa_period-plan(4).
          CONCATENATE l_anio wa_period-plan+4(3) INTO o_plan.
        ENDIF.


    ENDCASE.


  ENDMETHOD.


  METHOD get_plan_version.
    o_plan_version = me->g_plan_version.
  ENDMETHOD.


  METHOD get_pxq.


    DATA: wa_result TYPE ty_wa_result_pxq,
          lt_claves TYPE ty_lt_claves,
          l_total_p TYPE ty_wa_result_pxq-/bic/zpl_upric,
          lt_aux    LIKE e_result.


    lt_claves[] = i_claves[].
    DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.

    SELECT *
    INTO TABLE @DATA(lt_montos_re)
    FROM zpl_gastos_real AS a
    FOR ALL ENTRIES IN @lt_claves
    WHERE a~comp_code = @lt_claves-comp_code
    AND   a~co_area   = @lt_claves-co_area
    AND  a~fiscper IN @me->gr_fiscper_pxq
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
      AND   a~fiscper IN @me->gr_fiscper_pxq
      AND   a~gl_account = @lt_claves-gl_account
      AND   a~costcenter = @lt_claves-costcenter
      AND   a~coorder    = @lt_claves-coorder.

    IF sy-subrc EQ 0.
      SORT lt_montos_pre BY comp_code fiscper gl_account costcenter coorder.
    ENDIF.


******** Recorre las Claves del SOURCE_PACKAGE, según el Tipo de Período toma el Gasto del Real o del Presup de Gasto
    LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).

      CLEAR: l_total_p.

      LOOP AT me->gt_period_pxq ASSIGNING FIELD-SYMBOL(<fs_perio>).

        CLEAR wa_result.
**           Inicializa wa_result (Tipo 1):
        IF <fs_perio>-tipo EQ 'R'.
          READ TABLE  lt_montos_re ASSIGNING FIELD-SYMBOL(<fs_result_re>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                           fiscper    = <fs_perio>-actual
                                                           gl_account = <fs_clave>-gl_account
                                                           costcenter = <fs_clave>-costcenter
                                                           coorder    = <fs_clave>-coorder
                                                       BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING <fs_result_re> TO wa_result.

            wa_result-/bic/zpl_gasto = abs( <fs_result_re>-gastos * 100 ).
            wa_result-/bic/zquantity = 1.

          ENDIF.

        ELSE.

          READ TABLE  lt_montos_pre ASSIGNING FIELD-SYMBOL(<fs_result_pres>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                           fiscper    = <fs_perio>-actual
                                                           gl_account = <fs_clave>-gl_account
                                                           costcenter = <fs_clave>-costcenter
                                                           coorder    = <fs_clave>-coorder
                                                             BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING <fs_result_pres> TO wa_result.
            wa_result-/bic/zpl_gasto = abs( <fs_result_pres>-gastos ).
            wa_result-/bic/zquantity = 1.

          ENDIF.
        ENDIF.

        IF sy-subrc EQ 0.

          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-unit    = me->c_unit          .
          wa_result-currency = me->c_pyg.
          wa_result-curtype = me->c_curtype.
          wa_result-/bic/zpl_pgral = 1.
          wa_result-version = me->c_version.
          wa_result-/bic/ztabla = me->c_plan.


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
*           Inicializa wa_result (Tipo 2):
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          wa_result-fiscper    = <fs_perio>-actual.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-unit    = me->c_unit.
          wa_result-currency = me->c_pyg.
          wa_result-curtype = '10'.
          wa_result-/bic/zpl_upric = 0.
          wa_result-/bic/zpl_qinpu = 1.
          wa_result-/bic/zpl_pgral = 1.
          wa_result-/bic/zpl_gasto = 0.
          wa_result-/bic/zquantity = 0.
          wa_result-/bic/ztabla = me->c_plan.
          wa_result-version = me->c_version.
        ENDIF.


        ADD wa_result-/bic/zpl_upric TO l_total_p.



        CALL METHOD me->get_resp(
          EXPORTING
            i_soc    = wa_result-comp_code
            i_soco   = wa_result-co_area
            i_cuenta = wa_result-gl_account
            i_cebe   = wa_result-costcenter
            i_orden  = wa_result-coorder
          IMPORTING
            o_user   = wa_result-username
                       ).


        CALL METHOD me->get_plan_period
          EXPORTING
            i_tipo   = 'PXQ'
            i_period = <fs_perio>-actual "Periodo de input/precarga
          IMPORTING
            o_plan   = wa_result-fiscper. "Periodo Plan


        wa_result-fiscyear = wa_result-fiscper(4).



*       APPEND wa_result TO lt_aux.
        APPEND wa_result TO e_result.
        CLEAR wa_result.

      ENDLOOP." Periodos

*     IF l_total_p GT 0.
*        APPEND LINES OF lt_aux TO e_result.
*      ENDIF.
***
*      clear: lt_aux[], l_total_p.
    ENDLOOP." Claves Source Package


  ENDMETHOD.


  METHOD get_pxq_gasto_actual.







  ENDMETHOD.


  METHOD get_pxq_init.


    DATA: wa_result TYPE ty_wa_result_pxq,
          lt_claves TYPE ty_lt_claves. "ordenada

    lt_claves[] = i_claves[].
    DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.

*



******** Recorre las Claves del SOURCE_PACKAGE
    LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


*     Agrega los 12 periodos para cada Clave:
      LOOP AT me->gt_period_pxq ASSIGNING FIELD-SYMBOL(<fs_perio>).

        CLEAR wa_result.
*
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
        wa_result-version = me->g_draft_version.


*        ENDIF.


        CALL METHOD me->get_tabla( IMPORTING o_tabla = wa_result-/bic/ztabla ).


        CALL METHOD me->get_resp(
          EXPORTING
            i_soc    = wa_result-comp_code
            i_soco   = wa_result-co_area
            i_cuenta = wa_result-gl_account
            i_cebe   = wa_result-costcenter
            i_orden  = wa_result-coorder
          IMPORTING
            o_user   = wa_result-username
                       ).


        CALL METHOD me->get_plan_period
          EXPORTING
            i_tipo   = 'PXQ'
            i_period = <fs_perio>-actual "Periodo de input/precarga
          IMPORTING
            o_plan   = wa_result-fiscper. "Periodo Plan


        wa_result-fiscyear = wa_result-fiscper(4).



        APPEND wa_result TO e_result.
        CLEAR wa_result.

      ENDLOOP." Periodos
    ENDLOOP." Claves Source Package



  ENDMETHOD.


  METHOD get_pxq_pc.
*Ejecución de Precalculo de PXQ solo para cargas desde DTP o Process Chain.

    DATA: wa_result           TYPE ty_wa_result_pxq,
          lt_claves           TYPE ty_lt_claves,
          l_total_p           TYPE ty_wa_result_pxq-/bic/zpl_upric,
          lt_aux              LIKE e_result,
          lr_fiscper          LIKE LINE OF me->gr_fiscper_pxq,
          lr_fiscper_real     LIKE me->gr_fiscper_pxq,
          lr_fiscper_pres     LIKE me->gr_fiscper_pxq,
          l_value_not_initial TYPE c VALUE ''.



    lt_claves[] = i_claves[].
    DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.


    LOOP AT me->gt_period_pxq ASSIGNING FIELD-SYMBOL(<fs_period>) WHERE tipo EQ 'R'.
      lr_fiscper-low = <fs_period>-actual.
*      lr_fiscper-high = <fs_period>-tipo.
      lr_fiscper-option = 'EQ'.
      lr_fiscper-sign   = 'I'.
      APPEND lr_fiscper TO lr_fiscper_real.
    ENDLOOP.



    IF lt_claves[] IS NOT INITIAL AND lr_fiscper_real[] IS NOT INITIAL.
      SELECT *
      INTO TABLE @DATA(lt_montos_re)
      FROM zpl_gastos_real AS a
      FOR ALL ENTRIES IN @lt_claves
      WHERE a~comp_code = @lt_claves-comp_code
      AND   a~co_area   = @lt_claves-co_area
      AND  a~fiscper IN @lr_fiscper_real "me->gr_fiscper_pxq
      AND   a~gl_account = @lt_claves-gl_account
      AND   a~costcenter = @lt_claves-costcenter
      AND   a~coorder    = @lt_claves-coorder.

      IF sy-subrc EQ 0.
        SORT lt_montos_re BY comp_code fiscper gl_account costcenter coorder.
      ENDIF.
    ENDIF.




    LOOP AT me->gt_period_pxq ASSIGNING <fs_period> WHERE tipo EQ 'P'.
      lr_fiscper-low = <fs_period>-actual.
*      lr_fiscper-high = <fs_period>-tipo.
      lr_fiscper-option = 'EQ'.
      lr_fiscper-sign   = 'I'.
      APPEND lr_fiscper TO lr_fiscper_pres.
    ENDLOOP.


    IF lt_claves[] IS NOT INITIAL AND lr_fiscper_pres[] IS NOT INITIAL.
      SELECT *
        INTO TABLE @DATA(lt_montos_pre)
        FROM zpl_gastos_pre AS a
        FOR ALL ENTRIES IN @lt_claves
        WHERE a~comp_code = @lt_claves-comp_code
        AND   a~co_area   = @lt_claves-co_area
        AND   a~fiscper IN @lr_fiscper_pres "me->gr_fiscper_pxq
        AND   a~gl_account = @lt_claves-gl_account
        AND   a~costcenter = @lt_claves-costcenter
        AND   a~coorder    = @lt_claves-coorder.

      IF sy-subrc EQ 0.
        SORT lt_montos_pre BY comp_code fiscper gl_account costcenter coorder.
      ENDIF.
    ENDIF.


******** Recorre las Claves del SOURCE_PACKAGE, según el Tipo de Período toma el Gasto del Real o del Presup de Gasto
    LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).

      CLEAR: l_total_p, l_value_not_initial.

      LOOP AT me->gt_period_pxq ASSIGNING FIELD-SYMBOL(<fs_perio>).

        CLEAR wa_result.
**           Inicializa wa_result (Tipo 1):
        IF <fs_perio>-tipo EQ 'R'.
          READ TABLE  lt_montos_re ASSIGNING FIELD-SYMBOL(<fs_result>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                           fiscper    = <fs_perio>-actual
                                                           gl_account = <fs_clave>-gl_account
                                                           costcenter = <fs_clave>-costcenter
                                                           coorder    = <fs_clave>-coorder
                                                       BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING <fs_result> TO wa_result.

            wa_result-/bic/zpl_gasto =  <fs_result>-gastos * 100.
            wa_result-/bic/zquantity = 1.

          ENDIF.

        ELSE.

          READ TABLE  lt_montos_pre ASSIGNING FIELD-SYMBOL(<fs_result_pres>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                           fiscper    = <fs_perio>-actual
                                                           gl_account = <fs_clave>-gl_account
                                                           costcenter = <fs_clave>-costcenter
                                                           coorder    = <fs_clave>-coorder
                                                             BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING <fs_result_pres> TO wa_result.
            wa_result-/bic/zpl_gasto = <fs_result_pres>-gastos.
            wa_result-/bic/zquantity = <fs_result_pres>-cantidad.

          ENDIF.
        ENDIF.

        IF sy-subrc EQ 0.

          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-fiscper    = <fs_perio>-plan.
          wa_result-unit    = me->c_unit          .
          wa_result-currency = me->c_pyg.
          wa_result-curtype = me->c_curtype.
          wa_result-/bic/zpl_pgral = 1.
          wa_result-version = i_version.
          wa_result-/bic/ztabla = i_tabla.


          IF wa_result-/bic/zquantity IS NOT INITIAL.
            wa_result-/bic/zpl_upric = wa_result-/bic/zpl_gasto / wa_result-/bic/zquantity.
            wa_result-/bic/zpl_qinpu = wa_result-/bic/zquantity.

          ELSEIF wa_result-/bic/zpl_gasto IS NOT INITIAL.
            wa_result-/bic/zpl_upric = wa_result-/bic/zpl_gasto.
            wa_result-/bic/zpl_qinpu = 1.
          ENDIF.

        ELSE.
*           Inicializa wa_result (Tipo 2):
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          wa_result-fiscper    = <fs_perio>-plan.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-unit    = me->c_unit.
          wa_result-currency = me->c_pyg.
          wa_result-curtype = me->c_curtype.
          wa_result-/bic/zpl_upric = 0.
          wa_result-/bic/zpl_qinpu = 1.
          wa_result-/bic/zpl_pgral = 1.
          wa_result-/bic/zpl_gasto = 0.
          wa_result-/bic/zquantity = 0.
          wa_result-/bic/ztabla = i_tabla.
          wa_result-version = i_version.
        ENDIF.

        IF wa_result-/bic/zpl_upric NE 0.
          l_value_not_initial = 'X'.
        ENDIF.

        ADD wa_result-/bic/zpl_upric TO l_total_p.



        CALL METHOD me->get_resp(
          EXPORTING
            i_soc    = wa_result-comp_code
            i_soco   = wa_result-co_area
            i_cuenta = wa_result-gl_account
            i_cebe   = wa_result-costcenter
            i_orden  = wa_result-coorder
          IMPORTING
            o_user   = wa_result-username
                       ).


*        CALL METHOD me->get_plan_period
*          EXPORTING
*            i_tipo   = 'PXQ'
*            i_period = <fs_perio>-actual "Periodo de input/precarga
*          IMPORTING
*            o_plan   = wa_result-fiscper. "Periodo Plan


        wa_result-fiscyear = wa_result-fiscper(4).



        APPEND wa_result TO lt_aux.
*        APPEND wa_result TO e_result.
        CLEAR wa_result.

      ENDLOOP." Periodos

*      IF l_total_p NE 0. "Cambio lógica: Si al menos 1 periodo del real tiene valor (negativo o positivo), se incluye la combinacion.
      IF l_value_not_initial EQ 'X'.
        APPEND LINES OF lt_aux TO e_result.
      ENDIF.
**
      CLEAR: lt_aux[], l_total_p, l_value_not_initial.
    ENDLOOP." Claves Source Package



  ENDMETHOD.


  METHOD get_resp.


    me->derive_user(
      EXPORTING
        i_soc    = i_soc
        i_soco   = i_soco
        i_cuenta = i_cuenta
        i_cebe   = i_cebe
        i_orden  = i_orden
      IMPORTING
        o_user   = o_user
        o_result = o_result
        o_msg    = o_msg
    ).


    IF o_user IS INITIAL.
      o_msg-msgid = 'CREATE'.
      o_msg-msgno = '1'.
      o_msg-msgty = 'E'.
      CONCATENATE 'No se encontro Resp. para la Combinacion: ' ` ` INTO o_msg-msgv1.
      o_msg-msgv2 = i_cuenta.
      o_msg-msgv3 = i_cebe.
      o_msg-msgv4 = i_orden.
      o_result = ' '.

    ELSE.
      o_msg-msgid = 'CREATE'.
      o_msg-msgno = '1'.
      o_msg-msgty = 'S'.
      CONCATENATE 'Responsable asignado' ` ` o_user '-' INTO o_msg-msgv1.
      o_msg-msgv2 = i_cuenta.
      o_msg-msgv3 = i_cebe.
      o_msg-msgv4 = i_orden.
      o_result = 'X'.
    ENDIF.



  ENDMETHOD.


  METHOD get_sap_user.

*IMPORTING i_ibm_user type char3
*                   exporting o_sap_suer type char12.

    READ TABLE gt_user ASSIGNING FIELD-SYMBOL(<fs_user>)
       WITH KEY user_ibm = i_ibm_user.
    IF sy-subrc EQ 0.
      o_sap_user = <fs_user>-user_sap.
    ENDIF.

  ENDMETHOD.


  METHOD get_tabla.
    o_tabla = me->g_ztabla.
  ENDMETHOD.


  METHOD get_var.
*********************************************************************************************
    DATA: o_utiles TYPE REF TO zpl_utiles.
    DATA: l_coef TYPE p DECIMALS 4.
    DATA: lr_fiscper_re  TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_pre TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper     LIKE LINE OF lr_fiscper_re.

    DATA: wa_result           TYPE ty_wa_result_var,
          lt_claves           TYPE ty_lt_claves, "ordenada
          lt_clave_venta      TYPE ty_lt_clave_venta,
          lt_aux              TYPE ty_lt_result_var,
          lt_glaccount        TYPE RANGE OF /bi0/oigl_account,
          l_value_not_initial TYPE /bic/oizpl_gasto VALUE 0.


    TRY.
        o_utiles =  NEW zpl_utiles( ).

        lt_claves[] = i_claves[].
        DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
*-----------------------------------------------------------------------------


        LOOP AT o_utiles->gt_period_var ASSIGNING FIELD-SYMBOL(<fs_period_var>).


          wr_fiscper-option = 'EQ'.
          wr_fiscper-sign   = 'I'.

*         Fuente para Gastos: Real o Presupuesto
          IF <fs_period_var>-tipo EQ 'R'.
            wr_fiscper-low = <fs_period_var>-actual.
            APPEND wr_fiscper TO lr_fiscper_re.
          ELSE.
            wr_fiscper-low = <fs_period_var>-actual.
            APPEND wr_fiscper TO lr_fiscper_pre.
          ENDIF.
        ENDLOOP.





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

*   1.2 Gasto Presupuestado
        IF lr_fiscper_pre[] IS NOT INITIAL.
          SELECT *
          INTO TABLE @DATA(lt_gastos_pre)
          FROM zpl_gastos_pre_var AS a
          FOR ALL ENTRIES IN @lt_claves
          WHERE a~comp_code EQ @lt_claves-comp_code
          AND   a~fiscper IN @lr_fiscper_pre
          AND   a~gl_account = @lt_claves-gl_account
          AND   a~costcenter = @lt_claves-costcenter
          AND   a~coorder    = @lt_claves-coorder
          AND   a~/bic/ztabla = @me->c_plan_actual.

          IF sy-subrc EQ 0.
            SORT lt_gastos_pre BY comp_code fiscper gl_account costcenter coorder.
          ENDIF.
        ENDIF.



*2) Obtengo para cada CEBE: Linea, Of de Venta y Marca


        SELECT
        b~co_area,
        b~costcenter,
        b~sales_off,
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
          DELETE lt_costcenter WHERE sales_off EQ '' AND matl_group EQ '' AND /bic/zmar EQ ''.
*---------------------------------------------------------------------------------
*27/02/2024: Mejora para tomar Ventas de Vehículos 0KM como Linea VH.
          LOOP AT lt_costcenter ASSIGNING FIELD-SYMBOL(<fs_costctr>).
            CASE <fs_costctr>-matl_group.
              WHEN 'VH'.
                <fs_costctr>-matl_group = 'VH'.
              WHEN 'V0'.
                <fs_costctr>-matl_group = 'VH'.
              WHEN 'VU'.
                <fs_costctr>-matl_group = 'VU'.
            ENDCASE.
*---------------------------------------------------------------------------------

          ENDLOOP.

          lt_clave_venta = CORRESPONDING #( lt_costcenter ).
          DELETE ADJACENT DUPLICATES FROM lt_clave_venta.

        ENDIF.

*--------------------------------------------------------------------------------------------
*3) VENTAS REALES Agrupadas por Linea, Of Vta y Marca - Fuente Reales CEBE (ZPCA_O01N)
*---------------------------------------------------------------------------------------------
*3.0) Busca la Venta Real. utilizando el CECO.
        IF lr_fiscper_re[] IS NOT INITIAL.
          SELECT
              a~comp_code,
              a~co_area,
              a~fiscper,
              a~cebe,
              a~ventastotal AS ventastotal,
              a~canttotal   AS canttotal
           INTO TABLE @DATA(lt_vta_real_ceco)
           FROM
            zpl_vta_real_cebe( i_datum = @sy-datum ) AS a
            FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~co_area EQ @lt_clave_venta-co_area
           AND  a~fiscper IN @lr_fiscper_re
           AND   a~cebe EQ @lt_clave_venta-costcenter.
          IF sy-subrc EQ 0.
            SORT lt_vta_real_ceco BY comp_code fiscper cebe.
          ENDIF.




*3.1) Selecciona los Reales para calcular coeficiente de Participacion del Gasto sobre la Venta


          SELECT * "Explicito el * para que la modificacion solo deba ser a nivel CDS
           INTO TABLE @DATA(lt_ventas_re)
           FROM
            zpl_cebes_real( i_datum = @sy-datum ) AS a
            FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~co_area EQ @lt_clave_venta-co_area
          AND   a~fiscper IN @lr_fiscper_re
          AND   a~sucursal EQ @lt_clave_venta-sales_off
          AND   a~linea EQ @lt_clave_venta-matl_group
          AND   a~marca    EQ @lt_clave_venta-/bic/zmar.

          IF sy-subrc EQ 0.
            SORT lt_ventas_re BY comp_code fiscper sucursal linea marca.
          ENDIF.


*3.1.2)   Busca las ventas agrupadas por Sucursal y Línea
          SELECT *
          INTO TABLE @DATA(lt_ventas_suc_lin)
          FROM
           zpl_vta_suc_linea( i_datum = @sy-datum ) AS a
           FOR ALL ENTRIES IN @lt_clave_venta
         WHERE a~co_area EQ @lt_clave_venta-co_area
         AND   a~fiscper IN @lr_fiscper_re
         AND   a~sucursal EQ @lt_clave_venta-sales_off
         AND   a~linea EQ @lt_clave_venta-matl_group
         AND   a~ventastotal NE 0.

          IF sy-subrc EQ 0.

            SORT lt_ventas_suc_lin BY comp_code fiscper sucursal linea.
          ENDIF.

*3.1.3)   Busca las ventas agrupadas por Sucursal
*        SELECT *
*        INTO TABLE @DATA(lt_ventas_suc_)
*        FROM
*         zpl_vta_suc_( i_datum = @sy-datum ) AS a
*         FOR ALL ENTRIES IN @lt_clave_venta
*       WHERE a~co_area EQ @lt_clave_venta-co_area
*       AND   a~fiscper IN @lr_fiscper_re
*        AND  a~sucursal EQ @lt_clave_venta-sales_off
*        AND  a~ventastotal NE 0.
*        IF sy-subrc EQ 0.
*          SORT lt_ventas_suc_ BY comp_code fiscper sucursal.
*        ENDIF.

*------------------------------------------------------------------------

        ENDIF.




*------------------------------------------------------------------------------------
***VENTAS PRESUPUESTADAS (fuente COPA - 24V0 por ejemplo) - ZPL_A36
*------------------------------------------------------------------------------------


        IF lr_fiscper_pre[] IS NOT INITIAL.
*1) Busca la Venta Presup. utilizando el CECO.
          SELECT
              a~comp_code,
              a~co_area,
              a~fiscper,
              a~cebe,
              a~ventas AS ventastotal,
              a~cant   AS canttotal
           INTO TABLE @DATA(lt_vta_pres_ceco)
           FROM
            zpl_cebes_pres( p_datum = @sy-datum ) AS a
            FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~co_area EQ @lt_clave_venta-co_area
           AND  a~fiscper IN @lr_fiscper_pre
           AND   a~cebe EQ @lt_clave_venta-costcenter
          AND   a~tipo EQ 'venta'.
          IF sy-subrc EQ 0.
            SORT lt_vta_pres_ceco BY comp_code fiscper cebe.
          ENDIF.


*2) Busca la venta por Sucursal, Linea y Marca

          SELECT
            a~comp_code,
            a~co_area,
            a~fiscper,
            a~linea,
            a~sucursal,
            a~marca,
            a~gastostotal,
            a~ventastotal
         INTO TABLE @DATA(lt_vta_pres_full)
         FROM
          zpl_cebes_pres_u( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @lt_clave_venta
        WHERE a~co_area EQ @lt_clave_venta-co_area
         AND  a~fiscper IN @lr_fiscper_pre
         AND   a~sucursal EQ @lt_clave_venta-sales_off
        AND   a~linea EQ @lt_clave_venta-matl_group
        AND   a~marca    EQ @lt_clave_venta-/bic/zmar
        AND   a~tipo EQ 'venta'.
          IF sy-subrc EQ 0.
            SORT lt_vta_pres_full BY comp_code fiscper sucursal linea marca.
          ENDIF.


*3) Busca Venta Presup por Sucursal y Linea
          SELECT *
            INTO TABLE @DATA(lt_vta_pres_lin_suc)
            FROM
             zpl_vtas_pres_suc_linea( i_datum = @sy-datum ) AS a
             FOR ALL ENTRIES IN @lt_clave_venta
           WHERE a~co_area EQ @lt_clave_venta-co_area
            AND  a~fiscper IN @lr_fiscper_pre
           AND   a~sucursal EQ @lt_clave_venta-sales_off
           AND   a~linea EQ @lt_clave_venta-matl_group.
          IF sy-subrc EQ 0.
            SORT lt_vta_pres_lin_suc BY comp_code fiscper sucursal linea .
          ENDIF.


*4) Busca Venta Presup por Sucursal
*        SELECT *
*          INTO TABLE @DATA(lt_vta_pres_suc)
*          FROM
*           zpl_vtas_pres_suc_( i_datum = @sy-datum ) AS a
*           FOR ALL ENTRIES IN @lt_clave_venta
*         WHERE  a~co_area EQ @lt_clave_venta-co_area
*         AND  a~fiscper IN @lr_fiscper_vta
*         AND   a~sucursal EQ @lt_clave_venta-sales_off.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_pres_suc BY comp_code fiscper sucursal.
*        ENDIF.
*------------------------------------------------------------------------
        ENDIF.





*5) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
        READ TABLE lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>)  INDEX 1.
        IF sy-subrc EQ 0.

          SELECT 'I' AS sign, 'EQ' AS option, a~gl_account
          INTO TABLE @lt_glaccount
          FROM /bic/azpl_a292 AS a
          WHERE a~username EQ @<fs_clave>-username
          AND   a~comp_code EQ @<fs_clave>-comp_code
          AND   a~/bic/zpl_dum01 EQ '1'.

        ENDIF.


**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************
        LOOP AT  lt_claves ASSIGNING <fs_clave>.


******* Agrupadores Sucursal, LInea Marca
          READ TABLE lt_costcenter ASSIGNING FIELD-SYMBOL(<fs_cost>) WITH KEY costcenter = <fs_clave>-costcenter.
          CHECK sy-subrc EQ 0.

          CLEAR: l_coef,  wa_result, l_value_not_initial.

*     Inicializa wa_result:
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          MOVE-CORRESPONDING <fs_cost> TO wa_result.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          CALL METHOD o_utiles->get_tabla( IMPORTING o_tabla = wa_result-/bic/ztabla ).

          wa_result-unit    = o_utiles->c_unit.
          wa_result-currency = o_utiles->c_pyg.
          wa_result-version = o_utiles->c_version.
          wa_result-version = o_utiles->c_plan.



*     Agrega los 12 periodos para cada Clave:
          LOOP AT o_utiles->gt_period_var ASSIGNING FIELD-SYMBOL(<fs_perio>).

            CLEAR: wa_result-/bic/zpl_coef, wa_result-/bic/zpl_gasto, wa_result-/bic/zpl_gastp, wa_result-/bic/zpl_part,
                    wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp, wa_result-/bic/zquantity.


*----------------------------------------*
****   PERIODOS REALES:
*----------------------------------------*

            IF <fs_perio>-tipo EQ 'R'.
              READ TABLE  lt_gastos_re ASSIGNING FIELD-SYMBOL(<fs_gasto>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                               fiscper    = <fs_perio>-actual
                                                               gl_account = <fs_clave>-gl_account
                                                               costcenter = <fs_clave>-costcenter
                                                               coorder    = <fs_clave>-coorder
                                                           BINARY SEARCH.

              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_gasto = <fs_gasto>-gastos * 100.
              ENDIF.





********VENTAS REALES:
*1)  Venta REal a Nivel CEBE/CECO
              READ TABLE lt_vta_real_ceco ASSIGNING FIELD-SYMBOL(<fs_vtaceco>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                                        fiscper = <fs_perio>-actual
                                                                                        cebe   = <fs_clave>-costcenter
                                                                                        BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_venta =  abs( <fs_vtaceco>-ventastotal * 100 ).
              ELSE.
*               1.1) Sucursal - Linea - Marca
                READ TABLE lt_ventas_re ASSIGNING FIELD-SYMBOL(<fs_venta>) WITH KEY  comp_code = <fs_clave>-comp_code
                                                                                 fiscper   = <fs_perio>-actual
                                                                                 sucursal  = <fs_cost>-sales_off
                                                                                 linea     = <fs_cost>-matl_group
                                                                                 marca     = <fs_cost>-/bic/zmar
                                                                                 BINARY SEARCH.

                IF sy-subrc EQ 0.
                  wa_result-/bic/zpl_venta = <fs_venta>-ventastotal * 100.

                ELSE.
*                 1.2) Busca las ventas agrupadas por Linea y Sucursal
                  READ TABLE lt_ventas_suc_lin ASSIGNING FIELD-SYMBOL(<fs_suc_lin>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_cost>-sales_off
                                                                               linea     = <fs_cost>-matl_group
                                                                               BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    wa_result-/bic/zpl_venta = <fs_suc_lin>-ventastotal * 100.
                  ELSE.
**                   Busca las ventas agrupadas por sucursal
*                  READ TABLE lt_ventas_suc_ ASSIGNING FIELD-SYMBOL(<fs_suc_>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                               fiscper   = <fs_perio>-actual
*                                                                               sucursal  = <fs_cost>-sales_off
*                                                                               BINARY SEARCH.
*                  IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN lt_glaccount.
*                    wa_result-/bic/zpl_venta = <fs_suc_>-ventastotal * 100.
*                  ENDIF."Sucursal
                  ENDIF."Linea, Sucursal
                ENDIF."Sucursal, Linea Marca
              ENDIF."CEBE/CECO


            ELSE. "Parte Presupuesto
*----------------------------------------*
* PERIODOS PRESUPUESTO (Gastos y Ventas)
*----------------------------------------*
* Gastos Presupuestados 25V0
              READ TABLE  lt_gastos_pre ASSIGNING FIELD-SYMBOL(<fs_gastopre>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                               fiscper    = <fs_perio>-actual
                                                               gl_account = <fs_clave>-gl_account
                                                               costcenter = <fs_clave>-costcenter
                                                               coorder    = <fs_clave>-coorder
                                                           BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_gasto = <fs_gastopre>-gastos.
              ENDIF.


*Venta resupuesto 25V0 (de COPA)
*           2.1) Vta presupuestada CECO
              READ TABLE lt_vta_pres_ceco ASSIGNING FIELD-SYMBOL(<fs_vtapresceco>) WITH KEY
                                                                         comp_code = <fs_clave>-comp_code
                                                                         fiscper   = <fs_perio>-actual
                                                                         cebe      = <fs_clave>-costcenter
                                                                         BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_ventp =  abs( <fs_vtapresceco>-ventastotal ).
              ELSE.
*              2.2) Sucursal - Linea - Marca
                READ TABLE lt_vta_pres_full ASSIGNING FIELD-SYMBOL(<fs_venta_pre_full>) WITH KEY  comp_code = <fs_clave>-comp_code
                                                                                 fiscper   = <fs_perio>-actual
                                                                                 sucursal  = <fs_cost>-sales_off
                                                                                 linea     = <fs_cost>-matl_group
                                                                                 marca     = <fs_cost>-/bic/zmar
                                                                                 BINARY SEARCH.

                IF sy-subrc EQ 0.
                  wa_result-/bic/zpl_venta = abs( <fs_venta_pre_full>-ventastotal ).

                ELSE.
*          2.3) Busca las ventas agrupadas por Linea y Sucursal
                  READ TABLE lt_vta_pres_lin_suc ASSIGNING FIELD-SYMBOL(<fs_suc_lin_pre>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_cost>-sales_off
                                                                               linea     = <fs_cost>-matl_group
                                                                               BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    wa_result-/bic/zpl_venta = abs( <fs_suc_lin_pre>-ventastotal ).
                  ELSE.
*            Busca las ventas agrupadas por sucursal
*                  READ TABLE lt_vta_pres_suc ASSIGNING FIELD-SYMBOL(<fs_suc_pre>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                               fiscper   = <fs_perio>-actual
*                                                                               sucursal  = <fs_cost>-sales_off
*                                                                               BINARY SEARCH.
*                  IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN lt_glaccount.
*                    wa_result-/bic/zpl_venta = <fs_suc_pre>-ventastotal * 100.
*                  ENDIF."Sucursal
                  ENDIF."CECO
                ENDIF."Linea, Sucursal
              ENDIF."Sucursal, Linea Marca

            ENDIF."REAL/PRESUP



            wa_result-/bic/zpl_venta = abs( wa_result-/bic/zpl_venta  ).
            wa_result-/bic/zpl_gasto = abs( wa_result-/bic/zpl_gasto  ).

            IF wa_result-/bic/zpl_venta <> 0.
              wa_result-/bic/zpl_coef =  wa_result-/bic/zpl_gasto / wa_result-/bic/zpl_venta.
            ELSE.
              wa_result-/bic/zpl_coef = 0.
            ENDIF.



           add wa_result-/bic/zpl_gasto to   l_value_not_initial.
*------------------------------------

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
                i_tipo   = 'VAR'
                i_period = <fs_perio>-actual "Periodo de input/precarga
              IMPORTING
                o_plan   = wa_result-fiscper. "Periodo Plan


            wa_result-fiscyear = wa_result-fiscper(4).


            APPEND wa_result TO lt_aux.

          ENDLOOP." Periodos

*         IF l_value_not_initial ne 0. "Solo para carga inicial
            APPEND LINES OF lt_aux TO e_result.
*          ENDIF.

          CLEAR: lt_aux[], l_value_not_initial.
        ENDLOOP." Claves Source Package

*        e_result = lt_aux[].

      CATCH  cx_root INTO DATA(l_excepcion).
    ENDTRY.
  ENDMETHOD.


  METHOD get_varq.

*********************************************************************************************
    DATA: o_utiles TYPE REF TO zpl_utiles.
    DATA: l_coef TYPE p DECIMALS 4.
    DATA: lr_fiscper_re  TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_pre TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper     LIKE LINE OF lr_fiscper_re,
          lt_glaccount   TYPE RANGE OF /bi0/oigl_account.


    DATA: wa_result           TYPE ty_wa_result_varq,
          lt_claves           TYPE ty_lt_claves, "ordenada
          lt_clave_venta      TYPE ty_lt_clave_venta,
          lt_aux              TYPE ty_lt_result_varq,
          l_value_not_initial TYPE /bic/oizpl_gasto VALUE 0.



    TRY.
        o_utiles =  NEW zpl_utiles( ).

        lt_claves[] = i_claves[].
        DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
*-----------------------------------------------------------------------------


        LOOP AT o_utiles->gt_period_varq ASSIGNING FIELD-SYMBOL(<fs_period_var>).


          wr_fiscper-low = <fs_period_var>-actual.
          wr_fiscper-option = 'EQ'.
          wr_fiscper-sign   = 'I'.

          IF <fs_period_var>-tipo EQ 'R'.
            APPEND wr_fiscper TO lr_fiscper_re.
          ELSE.
            APPEND wr_fiscper TO lr_fiscper_pre.
          ENDIF.
        ENDLOOP.


* *1) GASTOS:  Nivel CUENTA CEBE ORDEN - Tomar Cantidades
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

        IF lr_fiscper_pre[] IS NOT INITIAL.
          SELECT *
          INTO TABLE @DATA(lt_gastos_pre)
          FROM zpl_gastos_pre_varq AS a
          FOR ALL ENTRIES IN @lt_claves
          WHERE a~fiscper IN @lr_fiscper_pre
          AND   a~gl_account = @lt_claves-gl_account
          AND   a~costcenter = @lt_claves-costcenter
          AND   a~coorder    = @lt_claves-coorder
          AND   a~/bic/ztabla = @me->c_plan_actual.

          IF sy-subrc EQ 0.
            SORT lt_gastos_pre BY comp_code fiscper gl_account costcenter coorder.
          ENDIF.
        ENDIF.


*2) Obtengo para cada CEBE: Linea, Of de Venta y Marca

*    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
*    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

        SELECT
        b~co_area,
        b~costcenter,
        b~sales_off,
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
          DELETE lt_costcenter WHERE sales_off EQ '' AND matl_group EQ '' AND /bic/zmar EQ ''.

          lt_clave_venta = CORRESPONDING #( lt_costcenter ).
          DELETE ADJACENT DUPLICATES FROM lt_clave_venta.
        ENDIF.




*----------------------------------------------------------------------------------------
*3) VENTAS PRESUPUESTADAS - 26V0
*----------------------------------------------------------------------------------------
*3.0) Busca la Venta Presup. utilizando el CECO.
        IF lr_fiscper_pre[] IS NOT INITIAL.

          SELECT
              a~comp_code,
              a~co_area,
              a~fiscper,
              a~cebe,
              a~ventas AS ventastotal,
              a~cant   AS canttotal
           INTO TABLE @DATA(lt_vta_pres_ceco)
           FROM
            zpl_cebes_pres( p_datum = @sy-datum ) AS a
            FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~co_area EQ @lt_clave_venta-co_area
           AND  a~fiscper IN @lr_fiscper_pre
           AND   a~cebe EQ @lt_clave_venta-costcenter
           AND   a~tipo EQ 'venta'
           AND   a~tabla = @me->c_plan_actual.
          IF sy-subrc EQ 0.
            SORT lt_vta_pres_ceco BY comp_code fiscper cebe.
          ENDIF.



*3.1) Ventas Presup por clave completa: Sucursal, LInea, Marca
          SELECT *
         INTO TABLE @DATA(lt_vtas_pres_suc_linea_marca)
         FROM
          zpl_cebes_pres_u( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~fiscper IN @lr_fiscper_pre
        AND   a~co_area EQ @lt_clave_venta-co_area
        AND   a~linea EQ @lt_clave_venta-matl_group
        AND   a~sucursal EQ @lt_clave_venta-sales_off
        AND   a~marca    EQ @lt_clave_venta-/bic/zmar
        AND   a~tipo EQ 'venta'
        AND   a~tabla = @me->c_plan_actual.
          IF sy-subrc EQ 0.
            SORT lt_vtas_pres_suc_linea_marca BY comp_code fiscper  sucursal linea marca.
          ENDIF.


*3.2)   Busca las ventas agrupadas por Sucursal y Línea
          SELECT *
          INTO TABLE @DATA(lt_vtas_pres_suc_lin)
          FROM
          zpl_vtas_pres_suc_linea( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~co_area EQ @lt_clave_venta-co_area
          AND   a~fiscper IN @lr_fiscper_pre
          AND   a~linea EQ @lt_clave_venta-matl_group
          AND   a~sucursal EQ @lt_clave_venta-sales_off
          AND   a~tabla = @me->c_plan_actual
          AND   a~ventastotal NE 0.

          IF sy-subrc EQ 0.
            SORT lt_vtas_pres_suc_lin BY comp_code fiscper sucursal linea .
          ENDIF.
*
**3.3)   Busca las ventas agrupadas por Sucursal
*    SELECT *
*    INTO TABLE @DATA(lt_vtas_pres_suc_)
*    FROM
*    zpl_vtas_pres_suc_( i_datum = @sy-datum ) AS a
*    FOR ALL ENTRIES IN @lt_clave_venta
*    WHERE a~co_area EQ @lt_clave_venta-co_area
*    AND   a~fiscper IN @lr_fiscper_vta
*    AND  a~sucursal EQ @lt_clave_venta-sales_off
*    AND  a~ventastotal NE 0.
*    IF sy-subrc EQ 0.
*      SORT lt_vtas_pres_suc_ BY comp_code fiscper sucursal.
*    ENDIF.

        ENDIF.




*----------------------------------------------------------------------------------------
*4) VENTAS REALES Agrupadas por Sucursal, Linea, y Marca - Fuente Reales CEBE (ZPCA_O01N)
*----------------------------------------------------------------------------------------

        IF lr_fiscper_re[] IS NOT INITIAL.
*4.1) Busca la Venta Real. utilizando el CECO.
          SELECT
              a~comp_code,
              a~co_area,
              a~fiscper,
              a~cebe,
              a~ventastotal AS ventastotal,
              a~canttotal   AS canttotal
           INTO TABLE @DATA(lt_vta_real_ceco)
           FROM
            zpl_vta_real_cebe( i_datum = @sy-datum ) AS a
            FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~co_area EQ @lt_clave_venta-co_area
           AND  a~fiscper IN @lr_fiscper_re
           AND   a~cebe EQ @lt_clave_venta-costcenter.
          IF sy-subrc EQ 0.
            SORT lt_vta_real_ceco BY comp_code fiscper cebe.
          ENDIF.




*4.2) Busca por Sucursal, Línea y Marca
          SELECT * "Explicito el * para que la modificacion solo deba ser a nivel CDS
           INTO TABLE @DATA(lt_vtas_real_suc_linea_marca)
           FROM
            zpl_cebes_real( i_datum = @sy-datum ) AS a
            FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~co_area EQ @lt_clave_venta-co_area
          AND   a~fiscper IN @lr_fiscper_re
          AND   a~sucursal EQ @lt_clave_venta-sales_off
          AND   a~linea EQ @lt_clave_venta-matl_group
          AND   a~marca    EQ @lt_clave_venta-/bic/zmar.

          IF sy-subrc EQ 0.
            SORT lt_vtas_real_suc_linea_marca BY comp_code fiscper sucursal linea marca.
          ENDIF.


*4.3)   Busca las ventas agrupadas por Sucursal y Línea
          SELECT *
          INTO TABLE @DATA(lt_vtas_real_suc_lin)
          FROM
           zpl_vta_suc_linea( i_datum = @sy-datum ) AS a
           FOR ALL ENTRIES IN @lt_clave_venta
         WHERE a~co_area EQ @lt_clave_venta-co_area
         AND   a~fiscper IN @lr_fiscper_re
         AND   a~sucursal EQ @lt_clave_venta-sales_off
         AND   a~linea EQ @lt_clave_venta-matl_group
         AND   a~ventastotal NE 0.

          IF sy-subrc EQ 0.

            SORT lt_vtas_real_suc_lin BY comp_code fiscper sucursal linea.
          ENDIF.



**5) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
*    READ TABLE lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>)  INDEX 1.
*    IF sy-subrc EQ 0.
*
*      SELECT 'I' AS sign, 'EQ' AS option, a~gl_account
*      INTO TABLE @lt_glaccount
*      FROM /bic/azpl_a292 AS a
*      WHERE a~username  EQ @<fs_clave>-username
*      AND   a~comp_code EQ @<fs_clave>-comp_code
*      AND   a~/bic/zpl_dum01 EQ '1'.
*
*    ENDIF.
*
        ENDIF.

**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************


        LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


******* Agrupadores Sucursal, LInea Marca
          READ TABLE lt_costcenter ASSIGNING FIELD-SYMBOL(<fs_costcenter>) WITH KEY costcenter = <fs_clave>-costcenter.
          CHECK sy-subrc EQ 0.

          CLEAR: l_coef,  wa_result, l_value_not_initial.

*     Inicializa wa_result:
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          MOVE-CORRESPONDING <fs_costcenter> TO wa_result.
          wa_result-costcenter = <fs_costcenter>-costcenter.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-/bic/ztabla = o_utiles->c_plan.
          wa_result-version = o_utiles->c_version.
          wa_result-unit    = o_utiles->c_unit.
          wa_result-currency = o_utiles->c_pyg.


*     Agrega los 12 periodos para cada Clave:
          LOOP AT o_utiles->gt_period_varq ASSIGNING FIELD-SYMBOL(<fs_perio>).

            CLEAR: wa_result-/bic/zpl_qinpu,
                   wa_result-/bic/zpl_upric,
                   wa_result-/bic/zpl_gasto,
                   wa_result-/bic/zpl_gastq,
                   wa_result-/bic/zpl_venta,
                   wa_result-/bic/zpl_vtaq,
                   wa_result-/bic/zpl_ventp,
                   wa_result-/bic/zpl_vtaqp.


*---------------------------------------
****   1. GASTOS y VENTA REALES:
*---------------------------------------
*---------------------------------------
*   1.2 GASTOS REALES:
*---------------------------------------

            IF <fs_perio>-tipo EQ 'R'.
              READ TABLE  lt_gastos_re ASSIGNING FIELD-SYMBOL(<fs_gasto>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                               fiscper    = <fs_perio>-actual
                                                               gl_account = <fs_clave>-gl_account
                                                               profit_ctr = <fs_clave>-costcenter
                                                               coorder    = <fs_clave>-coorder
                                                           BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_gasto = abs( <fs_gasto>-gastos * 100 ).
                wa_result-/bic/zpl_gastq = abs( <fs_gasto>-cantidad ).
              ENDIF.

*----------------------------------------------------------------------------*
* 1.2 VENTA REAL:
*----------------------------------------------------------------------------*
*       1.2.1) Busca solo por CECO:
              READ TABLE lt_vta_real_ceco ASSIGNING FIELD-SYMBOL(<fs_vta_real_ceco>) WITH KEY
                                                                          comp_code = <fs_clave>-comp_code
                                                                          fiscper   = <fs_perio>-actual
                                                                          cebe      = <fs_clave>-costcenter
                                                                          BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_venta =  abs( <fs_vta_real_ceco>-ventastotal ).
                wa_result-/bic/zpl_vtaq = abs( <fs_vta_real_ceco>-canttotal ).
              ELSE.

*         1.2.2) Busca por Sucursal, linea y marca:
                READ TABLE lt_vtas_real_suc_linea_marca ASSIGNING FIELD-SYMBOL(<fs_vta_real_full>) WITH KEY
                                                                               comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                                sucursal  = <fs_costcenter>-sales_off
                                                                               linea     = <fs_costcenter>-matl_group
                                                                               marca     = <fs_costcenter>-/bic/zmar BINARY SEARCH.
                IF sy-subrc EQ 0.

                  wa_result-/bic/zpl_venta =  abs( <fs_vta_real_full>-ventastotal  ).
                  wa_result-/bic/zpl_vtaq = abs( <fs_vta_real_full>-canttotal ).

                ELSE.
*           1.2.3) Busca por Linea y Sucursal
                  READ TABLE lt_vtas_real_suc_lin ASSIGNING FIELD-SYMBOL(<fs_vta_real_suc_lin>) WITH KEY
                                                                               comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_costcenter>-sales_off
                                                                               linea     = <fs_costcenter>-matl_group
                                                                               BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    wa_result-/bic/zpl_venta = abs( <fs_vta_real_suc_lin>-ventastotal ).
                    wa_result-/bic/zpl_vtaq  = abs( <fs_vta_real_suc_lin>-canttotal ).
                  ELSE.
*            Busca las ventas agrupadas por sucursal
*                READ TABLE lt_vtas_pres_suc_ ASSIGNING FIELD-SYMBOL(<fs_pres_suc_>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                             fiscper   = <fs_perio>-actual "Cambiar por Plan
*                                                                             sucursal  = <fs_costcenter>-sales_off
*                                                                             BINARY SEARCH.
*                IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN o_utiles->gt_cuentas.
*                  wa_result-/bic/zpl_ventp = abs( <fs_pres_suc_>-ventastotal * 100 ).
*                  wa_result-/bic/zpl_vtaqp  = abs( <fs_pres_suc_>-canttotal ).
*                ENDIF."Sucursal
                  ENDIF."Linea, Sucursal
                ENDIF."Sucursal, Linea Marca
              ENDIF."CECO




            ELSE."El período es Presupuesto
*----------------------------------------------------------------------------*
* 2. GASTO y VENTA PRESUPUESTADA:
*----------------------------------------------------------------------------*

*        2.1) Gasto Presupuestado
              READ TABLE  lt_gastos_pre ASSIGNING FIELD-SYMBOL(<fs_gastopre>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                               fiscper    = <fs_perio>-actual
                                                               gl_account = <fs_clave>-gl_account
                                                               costcenter = <fs_clave>-costcenter
                                                               coorder    = <fs_clave>-coorder
                                                           BINARY SEARCH.

              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_gasto = abs( <fs_gastopre>-gastos * 100 ).
                wa_result-/bic/zpl_gastq = abs( <fs_gastopre>-cantidad ).
              ENDIF.


*----------------------------------------------------------------------------*
*2.2 VENTA PRESUPUESTADA:
*----------------------------------------------------------------------------a
*       2.2.1) Busca solo por CECO:
              READ TABLE lt_vta_pres_ceco ASSIGNING FIELD-SYMBOL(<fs_vta_pres_ceco>) WITH KEY
                                                                          comp_code = <fs_clave>-comp_code
                                                                          fiscper   = <fs_perio>-actual
                                                                          cebe      = <fs_clave>-costcenter
                                                                          BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_venta =  abs( <fs_vta_pres_ceco>-ventastotal ).
                wa_result-/bic/zpl_vtaq = abs( <fs_vta_pres_ceco>-canttotal ).

              ELSE.

*           2.2.2) Busca por Sucursal, linea y marca:
                READ TABLE lt_vtas_pres_suc_linea_marca ASSIGNING FIELD-SYMBOL(<fs_vta_pres_full>) WITH KEY
                                                                               comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_costcenter>-sales_off
                                                                               linea     = <fs_costcenter>-matl_group
                                                                               marca     = <fs_costcenter>-/bic/zmar BINARY SEARCH.
                IF sy-subrc EQ 0.

                  wa_result-/bic/zpl_venta =  abs( <fs_vta_pres_full>-ventastotal  ).
                  wa_result-/bic/zpl_vtaq  = abs( <fs_vta_pres_full>-canttotal ).

*
                  IF sy-subrc EQ 0.

                    wa_result-/bic/zpl_venta =  abs( <fs_vta_real_full>-ventastotal  ).
                    wa_result-/bic/zpl_vtaq  = abs( <fs_vta_real_full>-canttotal ).

*            wa_result-/bic/zpl_gastp =  abs( <fs_presup>-gastostotal ).Todavía no hay datos de presupuesto gastos
                  ELSE.
*       2.3) Busca las ventas PRESUPUESTADAS agrupadas por Linea y Sucursal
                    READ TABLE lt_vtas_pres_suc_lin ASSIGNING FIELD-SYMBOL(<fs_pres_suc_lin>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                                 fiscper   = <fs_perio>-actual
                                                                                 sucursal  = <fs_costcenter>-sales_off
                                                                                 linea     = <fs_costcenter>-matl_group
                                                                                 BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      wa_result-/bic/zpl_venta = abs( <fs_pres_suc_lin>-ventastotal ).
                      wa_result-/bic/zpl_vtaq  = abs( <fs_pres_suc_lin>-canttotal ).

                    ELSE.
**            Busca las ventas agrupadas por sucursal
*            READ TABLE lt_vtas_pres_suc_ ASSIGNING FIELD-SYMBOL(<fs_pres_suc_>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                         fiscper   = <fs_perio>-actual "Cambiar por Plan
*                                                                         sucursal  = <fs_costcenter>-sales_off
*                                                                         BINARY SEARCH.
*            IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN o_utiles->gt_cuentas.
*              wa_result-/bic/zpl_ventp = abs( <fs_pres_suc_>-ventastotal * 100 ).
*              wa_result-/bic/zpl_vtaqp  = abs( <fs_pres_suc_>-canttotal ).
*            ENDIF."Sucursal
                    ENDIF.
                  ENDIF."Linea, Sucursal
                ENDIF."Sucursal, Linea Marca
              ENDIF."CECO

            ENDIF."Real/Presupuestado

*--------------------------------------
* Gasto Real / Cantidad Real = Precio Unitario o Monto Unitario
            IF wa_result-/bic/zpl_vtaq <> 0.
              wa_result-/bic/zpl_upric = wa_result-/bic/zpl_gasto / wa_result-/bic/zpl_vtaq.
            ELSE.
              wa_result-/bic/zpl_upric  = 0.
            ENDIF.


           add wa_result-/bic/zpl_gasto to   l_value_not_initial.
*--------------------------------------------------------


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
                i_tipo   = 'VARQ'
                i_period = <fs_perio>-actual "Periodo de input/precarga
              IMPORTING
                o_plan   = wa_result-fiscper. "Periodo Plan


            wa_result-fiscyear = wa_result-fiscper(4).

            APPEND wa_result TO lt_aux.
*       Clear wa_result se hace al inicio de cada loop periodos.

          ENDLOOP." Periodos


*          IF l_value_not_initial ne 0."Solo para carga inicial
            APPEND LINES OF lt_aux TO e_result.
*          ENDIF.

          CLEAR: lt_aux[], l_value_not_initial.
        ENDLOOP." Claves Source Package

*        e_result = lt_aux[].

      CATCH  cx_root INTO DATA(l_excepcion).

    ENDTRY.

  ENDMETHOD.


  METHOD get_varq_2000.




*********************************************************************************************
    DATA: o_utiles TYPE REF TO zpl_utiles.
    DATA: l_coef TYPE p DECIMALS 4.
    DATA: lr_fiscper_re  TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_pre TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper     LIKE LINE OF lr_fiscper_re,
          lt_glaccount   TYPE RANGE OF /bi0/oigl_account.


    DATA: wa_result           TYPE ty_wa_result_varq,
          lt_claves           TYPE ty_lt_claves, "ordenada
          lt_clave_venta      TYPE ty_lt_clave_venta_2000,
          lt_aux              TYPE ty_lt_result_varq,
          l_value_not_initial TYPE /bic/oizpl_gasto VALUE 0.



    TRY.
        o_utiles =  NEW zpl_utiles( ).

        lt_claves[] = i_claves[].
        DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
*-----------------------------------------------------------------------------


        LOOP AT o_utiles->gt_period_varq_2000 ASSIGNING FIELD-SYMBOL(<fs_period_var>).


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

        IF lr_fiscper_pre[] IS NOT INITIAL.
          SELECT *
          INTO TABLE @DATA(lt_gastos_pre)
          FROM zpl_gastos_pre_varq AS a
          FOR ALL ENTRIES IN @lt_claves
          WHERE a~fiscper IN @lr_fiscper_pre
          AND   a~gl_account = @lt_claves-gl_account
          AND   a~costcenter = @lt_claves-costcenter
          AND   a~coorder    = @lt_claves-coorder
          AND   a~/bic/ztabla = @me->c_plan_actual.

          IF sy-subrc EQ 0.
            SORT lt_gastos_pre BY comp_code fiscper gl_account costcenter coorder.
          ENDIF.
        ENDIF.


*2) Obtengo para cada CEBE: Linea, Of de Venta y Marca

*    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
*    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

        SELECT
        b~co_area,
        b~costcenter,
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

          lt_clave_venta = CORRESPONDING #( lt_costcenter ).
          DELETE ADJACENT DUPLICATES FROM lt_clave_venta.
        ENDIF.




*----------------------------------------------------------------------------------------
*3) VENTAS PRESUPUESTADAS - 26V0
*----------------------------------------------------------------------------------------
        IF lr_fiscper_pre[] IS NOT INITIAL.
*3.0) Busca la Venta Presup. utilizando el CECO.
          SELECT
                   a~comp_code,
                   a~co_area,
                   a~fiscper,
                   a~cebe,
                   a~ventas AS ventastotal,
                   a~cant   AS canttotal
                INTO TABLE @DATA(lt_vta_pres_ceco)
                FROM
                 zpl_cebes_pres( p_datum = @sy-datum ) AS a
                 FOR ALL ENTRIES IN @lt_clave_venta
               WHERE a~co_area EQ @lt_clave_venta-co_area
                AND  a~fiscper IN @lr_fiscper_pre
                AND   a~cebe EQ @lt_clave_venta-costcenter
                AND   a~tipo EQ 'venta'
                AND   a~tabla = @me->c_plan_actual.
          IF sy-subrc EQ 0.
            SORT lt_vta_pres_ceco BY comp_code fiscper cebe.
          ENDIF.


*3.1) Ventas Presup por clave completa: Sucursal, Canal, LInea, Marca
          SELECT *
         INTO TABLE @DATA(lt_vta_pres_s1)
         FROM
          zpl_vta_pres_s2_01_sum( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~fiscper IN @lr_fiscper_pre
        AND   a~co_area EQ @lt_clave_venta-co_area
        AND   a~sucursal EQ @lt_clave_venta-sales_off
        AND   a~canal    EQ @lt_clave_venta-distr_chan
        AND   a~linea EQ @lt_clave_venta-matl_group
        AND   a~marca    EQ @lt_clave_venta-/bic/zmar
        AND   a~tipo EQ 'venta'
        AND   a~tabla EQ @me->c_plan.
          IF sy-subrc EQ 0.
            SORT lt_vta_pres_s1 BY comp_code fiscper sucursal canal linea marca.
          ENDIF.


*3.2)   Busca las ventas agrupadas por Sucursal, Canal y Línea:S2
          SELECT *
          INTO TABLE @DATA(lt_vta_pres_s2)
          FROM
          zpl_vta_pres_s2_02_sum( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~co_area EQ @lt_clave_venta-co_area
          AND   a~fiscper IN @lr_fiscper_pre
          AND   a~sucursal EQ @lt_clave_venta-sales_off
          AND   a~canal    EQ @lt_clave_venta-distr_chan
          AND   a~linea EQ @lt_clave_venta-matl_group
          AND   a~tabla EQ @me->c_plan_actual
          AND   a~ventastotal NE 0.

          IF sy-subrc EQ 0.
            SORT lt_vta_pres_s2 BY comp_code fiscper linea sucursal.
          ENDIF.
*
**3.3)  Sucursal, Canal: S3
*    SELECT *
*    INTO TABLE @DATA(lt_vta_pres_s3)
*    FROM
*    zpl_vta_pres_s2_03_sum( i_datum = @sy-datum ) AS a
*    FOR ALL ENTRIES IN @lt_clave_venta
*    WHERE a~co_area EQ @lt_clave_venta-co_area
*    AND   a~fiscper IN @lr_fiscper_vta
*    AND  a~sucursal EQ @lt_clave_venta-sales_off
*    AND  a~canal    EQ @lt_clave_venta-distr_chan
*    AND  a~ventastotal NE 0.
*    IF sy-subrc EQ 0.
*      SORT lt_vta_pres_s3 BY comp_code fiscper sucursal canal.
*    ENDIF.

*
**3.4) Sucursal: S4
*    SELECT *
*      INTO TABLE @DATA(lt_vta_pres_s4)
*      FROM
*       zpl_vta_pres_s2_04_sum( i_datum = @sy-datum ) AS a
*       FOR ALL ENTRIES IN @lt_clave_venta
*     WHERE  a~co_area EQ @lt_clave_venta-co_area
*     AND  a~fiscper IN @lr_fiscper_vta
*     AND   a~sucursal EQ @lt_clave_venta-sales_off.
*    IF sy-subrc EQ 0.
*      SORT lt_vta_pres_s4 BY comp_code fiscper sucursal.
*    ENDIF.
        ENDIF.



*----------------------------------------------------------------------------------------
*4) VENTAS REALES Agrupadas por Sucursal, Canal, Linea y Marca - Fuente Reales CEBE (ZPCA_O01N)
*----------------------------------------------------------------------------------------

        IF lr_fiscper_re[] IS NOT INITIAL.
*4.1) Busca la Venta Real. utilizando el CECO.
          SELECT
             a~comp_code,
             a~co_area,
             a~fiscper,
             a~cebe,
             a~ventastotal AS ventastotal,
             a~canttotal   AS canttotal
          INTO TABLE @DATA(lt_vta_real_ceco)
          FROM
           zpl_vta_real_cebe( i_datum = @sy-datum ) AS a
           FOR ALL ENTRIES IN @lt_clave_venta
         WHERE a~co_area EQ @lt_clave_venta-co_area
          AND  a~fiscper IN @lr_fiscper_re
          AND   a~cebe EQ @lt_clave_venta-costcenter.
          IF sy-subrc EQ 0.
            SORT lt_vta_real_ceco BY comp_code fiscper cebe.
          ENDIF.




*4.2) Busca por Sucursal, canal, Línea y Marca
          SELECT *
      INTO TABLE @DATA(lt_vta_real_s1)
      FROM
       zpl_vta_real_s2_01_sum( i_datum = @sy-datum ) AS a
       FOR ALL ENTRIES IN @lt_clave_venta
       WHERE a~fiscper IN @lr_fiscper_re
     AND   a~co_area EQ @lt_clave_venta-co_area
     AND   a~sucursal EQ @lt_clave_venta-sales_off
     AND   a~canal    EQ @lt_clave_venta-distr_chan
     AND   a~linea EQ @lt_clave_venta-matl_group
     AND   a~marca    EQ @lt_clave_venta-/bic/zmar
     AND   a~tipo EQ 'venta'.
          IF sy-subrc EQ 0.
            SORT lt_vta_real_s1 BY comp_code fiscper sucursal canal linea marca.
          ENDIF.

*4.3)   Busca las ventas agrupadas por Sucursal, canal y Línea
          SELECT *
            INTO TABLE @DATA(lt_vta_real_s2)
            FROM
            zpl_vta_real_s2_02_sum( i_datum = @sy-datum ) AS a
            FOR ALL ENTRIES IN @lt_clave_venta
            WHERE a~co_area EQ @lt_clave_venta-co_area
            AND   a~fiscper IN @lr_fiscper_re
            AND   a~sucursal EQ @lt_clave_venta-sales_off
            AND   a~canal    EQ @lt_clave_venta-distr_chan
            AND   a~linea EQ @lt_clave_venta-matl_group
            AND   a~ventastotal NE 0.

          IF sy-subrc EQ 0.
            SORT lt_vta_real_s2 BY comp_code fiscper sucursal canal linea.
          ENDIF.

        ENDIF.



*4) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
        READ TABLE lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>)  INDEX 1.
        IF sy-subrc EQ 0.

          SELECT 'I' AS sign, 'EQ' AS option, a~gl_account
          INTO TABLE @lt_glaccount
          FROM /bic/azpl_a292 AS a
          WHERE a~username  EQ @<fs_clave>-username
          AND   a~comp_code EQ @<fs_clave>-comp_code
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
          wa_result-costcenter = <fs_costcenter>-costcenter.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-/bic/ztabla = o_utiles->c_plan.
          wa_result-unit    = o_utiles->c_unit.
          wa_result-currency = o_utiles->c_pyg.
          wa_result-version = o_utiles->c_version.


*     Agrega los 12 periodos para cada Clave:
          LOOP AT o_utiles->gt_period_varq_2000 ASSIGNING FIELD-SYMBOL(<fs_perio>).

            CLEAR: wa_result-/bic/zpl_qinpu,
                   wa_result-/bic/zpl_venta,
                   wa_result-/bic/zpl_ventp,
                   wa_result-/bic/zpl_venta,
                   wa_result-/bic/zpl_ventp,
                   wa_result-/bic/zpl_upric.

*---------------------------------------
****   1. GASTOS y VENTA REALES:
*---------------------------------------

*---------------------------------------
*   1.2 GASTOS REALES:
*---------------------------------------

            IF <fs_perio>-tipo EQ 'R'.
              READ TABLE  lt_gastos_re ASSIGNING FIELD-SYMBOL(<fs_gastore>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                               fiscper    = <fs_perio>-actual
                                                               gl_account = <fs_clave>-gl_account
                                                               profit_ctr = <fs_clave>-costcenter
                                                               coorder    = <fs_clave>-coorder
                                                           BINARY SEARCH.

              IF sy-subrc EQ 0.
                DATA(l_gasto)    = abs( <fs_gastore>-gastos * 100 ).
                DATA(l_gastoq)   = abs( <fs_gastore>-cantidad ).
              ENDIF.

*----------------------------------------------------------------------------*
* 1.2 VENTA REAL:
*----------------------------------------------------------------------------*

*       1.2.1) Busca solo por CECO:
              READ TABLE lt_vta_real_ceco ASSIGNING FIELD-SYMBOL(<fs_vta_real_ceco>) WITH KEY
                                                                          comp_code = <fs_clave>-comp_code
                                                                          fiscper   = <fs_perio>-actual
                                                                          cebe      = <fs_clave>-costcenter
                                                                          BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_venta =  abs( <fs_vta_real_ceco>-ventastotal ).
                wa_result-/bic/zpl_vtaq = abs( <fs_vta_real_ceco>-canttotal ).
              ELSE.

*         1.2.2) Busca por Sucursal, Canal, linea y marca:
                READ TABLE lt_vta_real_s1 ASSIGNING FIELD-SYMBOL(<fs_vta_real_s1>) WITH KEY
                                                                               comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_costcenter>-sales_off
                                                                               canal     = <fs_costcenter>-distr_chan
                                                                               linea     = <fs_costcenter>-matl_group
                                                                               marca     = <fs_costcenter>-/bic/zmar BINARY SEARCH.
                IF sy-subrc EQ 0.

                  wa_result-/bic/zpl_venta =  abs( <fs_vta_real_s1>-ventastotal  ).
                  wa_result-/bic/zpl_vtaq = abs( <fs_vta_real_s1>-canttotal ).

                ELSE.
*           1.2.3) Busca por Sucursal, Canal y  Linea
                  READ TABLE lt_vta_real_s2 ASSIGNING FIELD-SYMBOL(<fs_vta_real_s2>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_costcenter>-sales_off
                                                                               canal     = <fs_costcenter>-distr_chan
                                                                               linea     = <fs_costcenter>-matl_group
                                                                               BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    wa_result-/bic/zpl_ventp = abs( <fs_vta_real_s2>-ventastotal ).
                    wa_result-/bic/zpl_vtaqp  = abs( <fs_vta_real_s2>-canttotal ).
                  ELSE.
*            Busca las ventas agrupadas por sucursal
*                READ TABLE lt_vtas_pres_suc_ ASSIGNING FIELD-SYMBOL(<fs_pres_suc_>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                             fiscper   = <fs_perio>-actual "Cambiar por Plan
*                                                                             sucursal  = <fs_costcenter>-sales_off
*                                                                             BINARY SEARCH.
*                IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN o_utiles->gt_cuentas.
*                  wa_result-/bic/zpl_ventp = abs( <fs_pres_suc_>-ventastotal * 100 ).
*                  wa_result-/bic/zpl_vtaqp  = abs( <fs_pres_suc_>-canttotal ).
*                ENDIF."Sucursal
                  ENDIF."Linea, Sucursal
                ENDIF."Sucursal, Linea Marca
              ENDIF."CECO


            ELSE."Periodo Real o Presupuesto
*----------------------------------------------------------------------------*
* 2. GASTO y VENTA PRESUPUESTADA:
*----------------------------------------------------------------------------*
              READ TABLE  lt_gastos_pre ASSIGNING FIELD-SYMBOL(<fs_gastopre>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                               fiscper    = <fs_perio>-actual
                                                               gl_account = <fs_clave>-gl_account
                                                               costcenter = <fs_clave>-costcenter
                                                               coorder    = <fs_clave>-coorder
                                                           BINARY SEARCH.



*----------------------------------------------------------------------------*
* VENTA PRESUPUESTADA 25V0:
*----------------------------------------------------------------------------*
*     2.2.1) Busca solo por CECO:
              READ TABLE lt_vta_pres_ceco ASSIGNING FIELD-SYMBOL(<fs_vta_pres_ceco>) WITH KEY
                                                                          comp_code = <fs_clave>-comp_code
                                                                          fiscper   = <fs_perio>-actual
                                                                          cebe      = <fs_clave>-costcenter
                                                                          BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_ventp =  abs( <fs_vta_pres_ceco>-ventastotal ).
                wa_result-/bic/zpl_vtaqp = abs( <fs_vta_pres_ceco>-canttotal ).

              ELSE.
*     Por Sucursal, Canal, Linea, Marca
                READ TABLE lt_vta_pres_s1 ASSIGNING FIELD-SYMBOL(<fs_venta_p1>) WITH KEY
                                                                               comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_costcenter>-sales_off
                                                                               canal     = <fs_costcenter>-distr_chan
                                                                               linea     = <fs_costcenter>-matl_group
                                                                               marca     = <fs_costcenter>-/bic/zmar BINARY SEARCH.
                IF sy-subrc EQ 0.

                  wa_result-/bic/zpl_ventp =  abs( <fs_venta_p1>-ventastotal ).
                  wa_result-/bic/zpl_vtaqp = abs( <fs_venta_p1>-canttotal ).
*            wa_result-/bic/zpl_gastp =  abs( <fs_presup>-gastostotal ).Todavía no hay datos de presupuesto gastos
                ELSE.
*          Busca las ventas PRESUPUESTADAS agrupadas por Sucursal, Canal, Linea
                  READ TABLE lt_vta_pres_s2 ASSIGNING FIELD-SYMBOL(<fs_venta_p2>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_costcenter>-sales_off
                                                                               canal     = <fs_costcenter>-distr_chan
                                                                               linea     = <fs_costcenter>-matl_group
                                                                               BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    wa_result-/bic/zpl_ventp = abs( <fs_venta_p2>-ventastotal ).
                    wa_result-/bic/zpl_vtaqp  = abs( <fs_venta_p2>-canttotal ).
                  ENDIF."Sucursal, Canal, Linea
                ENDIF."Sucursal, Canal, Linea, Marca
              ENDIF."solo por CECO:



            ENDIF.

            IF sy-subrc EQ 0.
              l_gasto    = abs( <fs_gastopre>-gastos * 100 ).
              l_gastoq   = abs( <fs_gastopre>-cantidad ).
            ENDIF.

*--------------------------------------

            IF l_gastoq <> 0.
              wa_result-/bic/zpl_upric = l_gasto / l_gastoq.
            ELSE.
              wa_result-/bic/zpl_upric  = 0.
            ENDIF.


           add l_gasto to   l_value_not_initial.

*------------------------------------

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
                i_tipo   = 'VARQ'
                i_period = <fs_perio>-actual "Periodo de input/precarga
              IMPORTING
                o_plan   = wa_result-fiscper. "Periodo Plan


            wa_result-fiscyear = wa_result-fiscper(4).

            APPEND wa_result TO lt_aux.

          ENDLOOP." Periodos

          IF l_value_not_initial ne 0.
            APPEND LINES OF lt_aux TO e_result.
          ENDIF.
          CLEAR: lt_aux[], l_value_not_initial.

        ENDLOOP." Claves Source Package


*        e_result = lt_aux[].

      CATCH  cx_root INTO DATA(l_excepcion).

    ENDTRY.

  ENDMETHOD.


  METHOD get_varq_gasto_actual.


*********************************************************************************************
    DATA: o_utiles TYPE REF TO zpl_utiles.
    DATA: l_coef TYPE p DECIMALS 4.
    DATA: lr_fiscper_re  TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_pre TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper     LIKE LINE OF lr_fiscper_re,
          lt_glaccount   TYPE RANGE OF /bi0/oigl_account.


    DATA: wa_result      TYPE ty_wa_result_varq,
          lt_claves      TYPE ty_lt_claves, "ordenada
          lt_clave_venta TYPE ty_lt_clave_venta,
          lt_aux         TYPE ty_lt_result_varq,



          try.
    o_utiles =  NEW zpl_utiles( ).

    lt_claves[] = i_claves[].
    DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
*-----------------------------------------------------------------------------

* Gastos Actuales: Toma los periodos Plan, como reales
    LOOP AT o_utiles->gt_period_varq ASSIGNING FIELD-SYMBOL(<fs_period_var>).



      wr_fiscper-option = 'EQ'.
      wr_fiscper-sign   = 'I'.

      "Período para Actuales de Gasto: Real o Plan:
      IF me->g_tipo_actual EQ 'R'.
        wr_fiscper-low = <fs_period_var>-actual.
      ELSEIF me->g_tipo_actual EQ 'P'.
        wr_fiscper-low = <fs_period_var>-plan.
      ENDIF.
      APPEND wr_fiscper TO lr_fiscper_re.
    ENDLOOP.


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



*2) Obtengo para cada CEBE: Linea, Of de Venta y Marca

*    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
*    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

    SELECT
    b~co_area,
    b~costcenter,
    b~sales_off,
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
      DELETE lt_costcenter WHERE sales_off EQ '' AND matl_group EQ '' AND /bic/zmar EQ ''.

      lt_clave_venta = CORRESPONDING #( lt_costcenter ).
      DELETE ADJACENT DUPLICATES FROM lt_clave_venta.
    ENDIF.


**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************


    LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


      CLEAR: l_coef,  wa_result.

*     Inicializa wa_result:
      MOVE-CORRESPONDING <fs_clave> TO wa_result.
      wa_result-chrt_accts = 'SQS'.
      wa_result-fiscvarnt  = 'K4'.
      wa_result-/bic/ztabla = me->c_actual.
      wa_result-unit    = 'UN'.
      wa_result-currency = 'PYG'.
      wa_result-version = o_utiles->c_version.


*     Agrega los 12 periodos para cada Clave:
      LOOP AT o_utiles->gt_period_varq ASSIGNING FIELD-SYMBOL(<fs_perio>).

        CLEAR: wa_result-/bic/zpl_qinpu, wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp,
                        wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp, wa_result-/bic/zpl_upric,
                        wa_result-/bic/zpl_gasto, wa_result-/bic/zpl_gastq.

*---------------------------------------
****   GASTOS REALES:

        IF  me->g_tipo_actual EQ 'R'."Toma del Periodo Actual/Real o del Plan
          READ TABLE  lt_gastos_re ASSIGNING FIELD-SYMBOL(<fs_gasto>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                           fiscper    = <fs_perio>-actual
                                                           gl_account = <fs_clave>-gl_account
                                                           profit_ctr = <fs_clave>-costcenter
                                                           coorder    = <fs_clave>-coorder
                                                       BINARY SEARCH.
        ELSE.

          READ TABLE  lt_gastos_re ASSIGNING <fs_gasto>  WITH KEY comp_code  = <fs_clave>-comp_code
                                                           fiscper    = <fs_perio>-plan
                                                           gl_account = <fs_clave>-gl_account
                                                           profit_ctr = <fs_clave>-costcenter
                                                           coorder    = <fs_clave>-coorder
                                                       BINARY SEARCH.
        ENDIF.

        IF sy-subrc EQ 0.
          wa_result-/bic/zpl_gasto = abs( <fs_gasto>-gastos * 100 ).
          wa_result-/bic/zpl_gastq   = abs( <fs_gasto>-cantidad ).
        ENDIF.

*--------------------------------------


        CALL METHOD o_utiles->get_actual_period
          EXPORTING
            i_tipo   = 'VARQ'
            i_period = <fs_perio>-actual "Periodo Actual
          IMPORTING
            o_actual = wa_result-fiscper. "Periodo Actual/Real


        wa_result-fiscyear = wa_result-fiscper(4).

        APPEND wa_result TO lt_aux.

      ENDLOOP." Periodos
    ENDLOOP." Claves Source Package

    e_result = lt_aux[].






  ENDMETHOD.


  METHOD get_varq_init.



  ENDMETHOD.


  METHOD get_var_2000.




    DATA:
      wa_result TYPE ty_wa_result_var,
      lt_aux    TYPE ty_lt_result_var.

    DATA: o_utiles            TYPE REF TO zpl_utiles,
          l_coef              TYPE p DECIMALS 4,
          lr_fiscper_re       TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_pre      TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_vta      TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper          LIKE LINE OF lr_fiscper_re,
          lt_glaccount        TYPE RANGE OF /bi0/oigl_account,
          lt_claves           TYPE ty_lt_claves, "ordenada
          lt_clave_venta      TYPE ty_lt_clave_venta_2000,
          l_value_not_initial TYPE /bic/oizpl_gasto VALUE 0.


    TRY.

        o_utiles =  NEW zpl_utiles( ).

        lt_claves[] = i_claves[].
        DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
*-----------------------------------------------------------------------------


        LOOP AT o_utiles->gt_period_var_2000 ASSIGNING FIELD-SYMBOL(<fs_period_var>).

          wr_fiscper-low = <fs_period_var>-actual.
          wr_fiscper-option = 'EQ'.
          wr_fiscper-sign   = 'I'.

          IF <fs_period_var>-tipo EQ 'R'.
            APPEND wr_fiscper TO lr_fiscper_re.
          ELSE.
            APPEND wr_fiscper TO lr_fiscper_pre.
          ENDIF.
        ENDLOOP.




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

*   1.2 Gasto Presupuesto 25V0
        IF lr_fiscper_pre[] IS NOT INITIAL.
          SELECT *
          INTO TABLE @DATA(lt_gastos_pre)
          FROM zpl_gastos_pre_var AS a
          FOR ALL ENTRIES IN @lt_claves
          WHERE a~comp_code EQ @lt_claves-comp_code
          AND   a~fiscper IN @lr_fiscper_pre
          AND   a~gl_account = @lt_claves-gl_account
          AND   a~costcenter = @lt_claves-costcenter
          AND   a~coorder    = @lt_claves-coorder
          AND   a~/bic/ztabla = @me->c_plan_actual.

          IF sy-subrc EQ 0.
            SORT lt_gastos_pre BY comp_code fiscper gl_account costcenter coorder.
          ENDIF.
        ENDIF.



*------------------------------------------------------------------------
*2) Obtengo para cada CEBE: Linea, Of de Venta y Marca


*        clear lr_fiscper_re[].
*
*        LOOP AT o_utiles->gt_period_vta ASSIGNING FIELD-SYMBOL(<fs_period_vta>).
*
*          wr_fiscper-low = <fs_period_vta>-actual.
*          wr_fiscper-option = 'EQ'.
*          wr_fiscper-sign   = 'I'.
*
*          IF <fs_period_var>-tipo EQ 'R'.
*            APPEND wr_fiscper TO lr_fiscper_re.
*          ENDIF.
*        ENDLOOP.



*    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
*    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

        SELECT
        b~co_area,
        b~costcenter,
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

          lt_clave_venta = CORRESPONDING #( lt_costcenter ).
          DELETE ADJACENT DUPLICATES FROM lt_clave_venta.
        ENDIF.

*------------------------------------------------------------------------


        IF lt_clave_venta[] IS NOT INITIAL.
*-----------------------------------------------------------
*3) Obtención de VENTAS REALES Agrupadas
*------------------------------------------------------------
*3.1) Busca la Venta Real. utilizando el CECO.
          IF lr_fiscper_re[] IS NOT INITIAL.

            SELECT
                a~comp_code,
                a~co_area,
                a~fiscper,
                a~cebe,
                a~ventastotal AS ventastotal,
                a~canttotal   AS canttotal
             INTO TABLE @DATA(lt_vta_real_ceco)
             FROM
              zpl_vta_real_cebe( i_datum = @sy-datum ) AS a
              FOR ALL ENTRIES IN @lt_clave_venta
            WHERE a~co_area EQ @lt_clave_venta-co_area
             AND  a~fiscper IN @lr_fiscper_re
             AND   a~cebe EQ @lt_clave_venta-costcenter.
            IF sy-subrc EQ 0.
              SORT lt_vta_real_ceco BY comp_code fiscper cebe.
            ENDIF.

*3.1) Venta Real por Sucursal, Canal, línea y Marca:
            SELECT * "
             INTO TABLE @DATA(lt_vta_real_s1)
             FROM
              zpl_vta_real_s2_01_sum( i_datum = @sy-datum ) AS a
              FOR ALL ENTRIES IN @lt_clave_venta
            WHERE a~co_area EQ @lt_clave_venta-co_area
            AND   a~fiscper IN @lr_fiscper_re
            AND   a~sucursal EQ @lt_clave_venta-sales_off
            AND   a~canal    EQ @lt_clave_venta-distr_chan
            AND   a~linea EQ @lt_clave_venta-matl_group
            AND   a~marca    EQ @lt_clave_venta-/bic/zmar.

            IF sy-subrc EQ 0.
              SORT lt_vta_real_s1 BY comp_code fiscper sucursal canal linea  marca.
            ENDIF.


*3.1.2)   Busca Venta Real agrupadas por Sucursal, Canal y Línea:
            SELECT *
            INTO TABLE @DATA(lt_vta_real_s2)
            FROM
             zpl_vta_real_s2_02_sum( i_datum = @sy-datum ) AS a
             FOR ALL ENTRIES IN @lt_clave_venta
           WHERE a~co_area EQ @lt_clave_venta-co_area
           AND   a~fiscper IN @lr_fiscper_re
           AND   a~sucursal EQ @lt_clave_venta-sales_off
            AND   a~canal    EQ @lt_clave_venta-distr_chan
            AND   a~linea EQ @lt_clave_venta-matl_group
           AND   a~ventastotal NE 0.

            IF sy-subrc EQ 0.
              SORT lt_vta_real_s2 BY comp_code fiscper sucursal canal linea.
            ENDIF.

*3.1.3)   Busca las ventas agrupadas por Sucursal, Canal
*            SELECT *
*              INTO TABLE @DATA(lt_vta_real_s3)
*              FROM
*               zpl_vta_real_s2_03_sum( i_datum = @sy-datum ) AS a
*               FOR ALL ENTRIES IN @lt_clave_venta
*             WHERE a~co_area EQ @lt_clave_venta-co_area
*             AND   a~fiscper IN @lr_fiscper_re
*              AND  a~sucursal EQ @lt_clave_venta-sales_off
*              AND   a~canal    EQ @lt_clave_venta-distr_chan
*              AND  a~ventastotal NE 0.
*            IF sy-subrc EQ 0.
*              SORT lt_vta_real_s3 BY comp_code fiscper sucursal canal.
*            ENDIF.

          ENDIF."
*
*
*
**3.1.4)   Busca las ventas agrupadas por Sucursal
*        SELECT *
*        INTO TABLE @DATA(lt_vta_real_s4)
*        FROM
*         zpl_vta_real_s2_04_sum( i_datum = @sy-datum ) AS a
*         FOR ALL ENTRIES IN @lt_clave_venta
*       WHERE a~co_area EQ @lt_clave_venta-co_area
*       AND   a~fiscper IN @lr_fiscper_re
*        AND  a~sucursal EQ @lt_clave_venta-sales_off
*        AND  a~ventastotal NE 0.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_real_s4 BY comp_code fiscper sucursal.
*        ENDIF.
*


*-----------------------------------------------------------------------
***VENTAS PRESUPUESTADAS (fuente COPA -  - ZCOP_A01 )
*-----------------------------------------------------------------------


          IF lr_fiscper_pre[] IS NOT INITIAL.

*4.0) Busca la Venta Presup. utilizando el CECO.
            SELECT
                a~comp_code,
                a~co_area,
                a~fiscper,
                a~cebe,
                a~ventas AS ventastotal,
                a~cant   AS canttotal
             INTO TABLE @DATA(lt_vta_pres_ceco)
             FROM
              zpl_cebes_pres( p_datum = @sy-datum ) AS a
              FOR ALL ENTRIES IN @lt_clave_venta
            WHERE a~co_area EQ @lt_clave_venta-co_area
             AND  a~fiscper IN @lr_fiscper_pre
             AND   a~cebe EQ @lt_clave_venta-costcenter
            AND   a~tipo EQ 'venta'.
            IF sy-subrc EQ 0.
              SORT lt_vta_pres_ceco BY comp_code fiscper cebe.
            ENDIF.


*4.1)Clave completa: Sucursal, Canal, Linea, Marca
            SELECT *
           INTO TABLE @DATA(lt_vta_pres_s1)
           FROM
            zpl_vta_pres_s2_01_sum( i_datum = @sy-datum ) AS a
            FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~co_area EQ @lt_clave_venta-co_area
           AND  a~fiscper IN @lr_fiscper_pre
          AND   a~sucursal EQ @lt_clave_venta-sales_off
          AND   a~canal    EQ @lt_clave_venta-distr_chan
          AND   a~linea EQ @lt_clave_venta-matl_group
          AND   a~marca    EQ @lt_clave_venta-/bic/zmar
          AND   a~tipo EQ 'venta'.
            IF sy-subrc EQ 0.
              SORT lt_vta_pres_s1 BY comp_code fiscper sucursal canal linea marca.
            ENDIF.


*4,2) Sucursal, Canal, Linea:S2
            SELECT *
              INTO TABLE @DATA(lt_vta_pres_s2)
              FROM
               zpl_vta_pres_s2_02_sum( i_datum = @sy-datum ) AS a
               FOR ALL ENTRIES IN @lt_clave_venta
             WHERE a~co_area EQ @lt_clave_venta-co_area
             AND   a~fiscper IN @lr_fiscper_vta
             AND   a~sucursal EQ @lt_clave_venta-sales_off
             AND   a~canal    EQ @lt_clave_venta-distr_chan
             AND   a~linea EQ @lt_clave_venta-matl_group.
            IF sy-subrc EQ 0.
              SORT lt_vta_pres_s2 BY comp_code fiscper sucursal canal linea.
            ENDIF.
*
*
*4.3) Sucursal, Canal: S3
*            SELECT *
*              INTO TABLE @DATA(lt_vta_pres_s3)
*              FROM
*               zpl_vta_pres_s2_03_sum( i_datum = @sy-datum ) AS a
*               FOR ALL ENTRIES IN @lt_clave_venta
*             WHERE  a~co_area EQ @lt_clave_venta-co_area
*             AND   a~fiscper IN @lr_fiscper_vta
*             AND   a~sucursal EQ @lt_clave_venta-sales_off
*             AND   a~canal    EQ @lt_clave_venta-distr_chan.
*            IF sy-subrc EQ 0.
*              SORT lt_vta_pres_s3 BY comp_code fiscper sucursal canal.
*            ENDIF.
*
**4.4) Sucursal: S4
*        SELECT *
*          INTO TABLE @DATA(lt_vta_pres_s4)
*          FROM
*           zpl_vta_pres_s2_04_sum( i_datum = @sy-datum ) AS a
*           FOR ALL ENTRIES IN @lt_clave_venta
*         WHERE  a~co_area EQ @lt_clave_venta-co_area
*         AND  a~fiscper IN @lr_fiscper_vta
*         AND   a~sucursal EQ @lt_clave_venta-sales_off.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_pres_s4 BY comp_code fiscper sucursal.
*        ENDIF.
          ENDIF.

        ENDIF."clave venta no INICIAL

        .


*5) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
        READ TABLE lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>)  INDEX 1.

        SELECT 'I' AS sign, 'EQ' AS option, a~gl_account
        INTO TABLE @lt_glaccount
        FROM /bic/azpl_a292 AS a
        WHERE a~username  EQ @<fs_clave>-username
        AND   a~comp_code EQ @<fs_clave>-comp_code
        AND   a~/bic/zpl_dum01 EQ '1'.





**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************
        LOOP AT  lt_claves ASSIGNING <fs_clave>.


******* Agrupadores Sucursal, LInea Marca

          READ TABLE lt_costcenter ASSIGNING FIELD-SYMBOL(<fs_cost>) WITH KEY costcenter = <fs_clave>-costcenter.
*          CHECK sy-subrc EQ 0.

          CLEAR: l_coef,  wa_result, l_value_not_initial.

*     Inicializa wa_result:
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          MOVE-CORRESPONDING <fs_cost> TO wa_result.
          wa_result-costcenter = <fs_cost>-costcenter.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-/bic/ztabla = o_utiles->c_plan.
          wa_result-unit    = o_utiles->c_unit.
          wa_result-currency = o_utiles->c_pyg.
          wa_result-version = o_utiles->c_version.



*     Agrega los 12 periodos para cada Clave:
          LOOP AT o_utiles->gt_period_var_2000 ASSIGNING FIELD-SYMBOL(<fs_perio>).

            CLEAR: wa_result-/bic/zpl_coef, wa_result-/bic/zpl_gasto, wa_result-/bic/zpl_gastp, wa_result-/bic/zpl_part,
                    wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp, wa_result-/bic/zquantity.

            IF <fs_perio>-tipo EQ 'R'.
*----------------------------------------*
* PERIODOS REALES
*----------------------------------------*



****   GASTOS REALES:

              READ TABLE  lt_gastos_re ASSIGNING FIELD-SYMBOL(<fs_gasto>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                               fiscper    = <fs_perio>-actual
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
*1)  Venta REal a Nivel CEBE/CECO
              READ TABLE lt_vta_real_ceco ASSIGNING FIELD-SYMBOL(<fs_vtarealceco>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                                        fiscper = <fs_perio>-actual
                                                                                        cebe   = <fs_clave>-costcenter
                                                                                        BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_venta =  abs( <fs_vtarealceco>-ventastotal * 100 ).
              ELSE.


                READ TABLE lt_vta_real_s1 ASSIGNING FIELD-SYMBOL(<fs_venta_s1>) WITH KEY  comp_code = <fs_clave>-comp_code
                                                                                 fiscper   = <fs_perio>-actual
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
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_cost>-sales_off
                                                                               canal     = <fs_cost>-distr_chan
                                                                               linea     = <fs_cost>-matl_group
                                                                               BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    wa_result-/bic/zpl_venta = <fs_venta_s2>-ventastotal * 100.
                  ELSE.
**            Busca las ventas agrupadas por sucursal, canal
*                  READ TABLE lt_vta_real_s3 ASSIGNING FIELD-SYMBOL(<fs_venta_s3>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                               fiscper   = <fs_perio>-actual
*                                                                               sucursal  = <fs_cost>-sales_off
*                                                                               canal     = <fs_cost>-distr_chan
*                                                                               BINARY SEARCH.
*                  IF sy-subrc EQ 0.
*                    wa_result-/bic/zpl_venta = <fs_venta_s3>-ventastotal * 100.

*                  ELSE.
**            Busca las ventas agrupadas por sucursal
*                    READ TABLE lt_vta_real_s4 ASSIGNING FIELD-SYMBOL(<fs_venta_s4>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                                 fiscper   = <fs_perio>-actual
*                                                                                 sucursal  = <fs_cost>-sales_off
*                                                                                 BINARY SEARCH.
*                    IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN lt_glaccount.
*                      wa_result-/bic/zpl_venta = <fs_venta_s4>-ventastotal * 100.
                  ENDIF."Sucursal
                ENDIF."Sucursal, Canal
              ENDIF."Sucursal, Canal, LInea
*                ENDIF."Sucursal, Linea Marca
*              ENDIF."CECO


            ELSE. "Parte Presupuesto, 23V0 por ejemplo
*----------------------------------------*
* PERIODOS RESUPUESTADAS
*----------------------------------------*
* Gasto Presupuesto
              READ TABLE  lt_gastos_pre ASSIGNING FIELD-SYMBOL(<fs_gastopre>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                               fiscper    = <fs_perio>-actual
                                                               gl_account = <fs_clave>-gl_account
                                                               costcenter = <fs_clave>-costcenter
                                                               coorder    = <fs_clave>-coorder
                                                           BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_gasto = <fs_gastopre>-gastos.
              ENDIF.


*Venta Presupuesto para cálculo del COEF:
              "Nivel 1: CECO
              READ TABLE lt_vta_pres_ceco ASSIGNING FIELD-SYMBOL(<fs_vtapresceco>) WITH KEY
                                                                        comp_code = <fs_clave>-comp_code
                                                                        fiscper   = <fs_perio>-actual
                                                                        cebe      = <fs_clave>-costcenter
                                                                        BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_venta =  abs( <fs_vtapresceco>-ventastotal ).
              ELSE.
                "Nivel 2: SUCURSAL, Canal, Linea, Marca
                READ TABLE lt_vta_pres_s1 ASSIGNING FIELD-SYMBOL(<fs_venta_p1>)  WITH KEY  comp_code = <fs_clave>-comp_code
                                                                                     fiscper   = <fs_perio>-actual
                                                                                     sucursal  = <fs_cost>-sales_off
                                                                                     canal     = <fs_cost>-distr_chan
                                                                                     linea     = <fs_cost>-matl_group
                                                                                     marca     = <fs_cost>-/bic/zmar
                                                                                     BINARY SEARCH.

                IF sy-subrc EQ 0.
                  wa_result-/bic/zpl_venta = <fs_venta_p1>-ventastotal.
                ELSE.
*                 Nivel 3: Busca las ventas agrupadas por  Sucursal, Canal, Linea
                  READ TABLE lt_vta_pres_s2 ASSIGNING FIELD-SYMBOL(<fs_venta_p2>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_cost>-sales_off
                                                                               canal     = <fs_cost>-distr_chan
                                                                               linea     = <fs_cost>-matl_group
                                                                               BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    wa_result-/bic/zpl_venta = <fs_venta_p2>-ventastotal.
                  ELSE.
**            Busca las ventas agrupadas por sucursal, canal
*                    READ TABLE lt_vta_pres_s3 ASSIGNING FIELD-SYMBOL(<fs_venta_p3>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                                 fiscper   = <fs_perio>-actual
*                                                                                 sucursal  = wa_cost-sales_off
*                                                                                 canal     = wa_cost-distr_chan
*                                                                                 BINARY SEARCH.
*                    IF sy-subrc EQ 0.
*                      wa_result-/bic/zpl_venta = <fs_venta_p3>-ventastotal.
*
**            Busca las ventas agrupadas por sucursal, canal
*                      READ TABLE lt_vta_real_s3 ASSIGNING FIELD-SYMBOL(<fs_venta_s3>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                                   fiscper   = <fs_perio>-actual
*                                                                                   sucursal  = wa_cost-sales_off
*                                                                                   canal     = wa_cost-distr_chan
*                                                                                   BINARY SEARCH.
*                      IF sy-subrc EQ 0.
*                        wa_result-/bic/zpl_venta = <fs_venta_s3>-ventastotal.
*
*                      ELSE.
*
*
*                      ENDIF.
*                    ELSE.
**            Busca las ventas agrupadas por sucursal
*                    READ TABLE lt_vta_pres_s4 ASSIGNING FIELD-SYMBOL(<fs_venta_p4>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                                 fiscper   = <fs_perio>-actual
*                                                                                 sucursal  = <fs_cost>-sales_off
*                                                                                 BINARY SEARCH.
*                    IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN lt_glaccount.
*                      wa_result-/bic/zpl_venta = <fs_venta_p4>-ventastotal * 100.
                  ENDIF. "CECO
                ENDIF."Sucursal
              ENDIF."Sucursal, Canal
*                  ENDIF."Sucursal, Canal, LInea
*                ENDIF."Sucursal, Linea Marca


            ENDIF."REAL/PRESUP





            wa_result-/bic/zpl_venta = abs( wa_result-/bic/zpl_venta  ).
            wa_result-/bic/zpl_ventp = abs( wa_result-/bic/zpl_ventp  ).
            wa_result-/bic/zpl_gasto = abs( wa_result-/bic/zpl_gasto  ).

            IF wa_result-/bic/zpl_venta <> 0.
              wa_result-/bic/zpl_coef =  wa_result-/bic/zpl_gasto / wa_result-/bic/zpl_venta.
            ELSE.
              wa_result-/bic/zpl_coef = 0.
            ENDIF.


              add wa_result-/bic/zpl_gasto to   l_value_not_initial.
*--------------------------------------------------------
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
                i_tipo   = 'VAR'
                i_period = <fs_perio>-actual "Periodo de input/precarga
              IMPORTING
                o_plan   = wa_result-fiscper. "Periodo Plan


            wa_result-fiscyear = wa_result-fiscper(4).

            APPEND wa_result TO lt_aux.

          ENDLOOP." Periodos

          IF l_value_not_initial ne 0.
            APPEND LINES OF lt_aux TO e_result.
          ENDIF.

          CLEAR: lt_aux[], l_value_not_initial.
        ENDLOOP." Claves Source Package

*        e_result = lt_aux[].

      CATCH  cx_root INTO DATA(l_excepcion).

    ENDTRY.
  ENDMETHOD.


  METHOD get_var_gasto_actual.


    DATA: o_utiles TYPE REF TO zpl_utiles.
    DATA: l_coef TYPE p DECIMALS 4.
    DATA: lr_fiscper_re  TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_pre TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper     LIKE LINE OF lr_fiscper_re.

    DATA: wa_result      TYPE ty_wa_result_var,
          lt_claves      TYPE ty_lt_claves, "ordenada
          lt_clave_venta TYPE ty_lt_clave_venta,
          lt_aux         TYPE ty_lt_result_var,
          lt_glaccount   TYPE RANGE OF /bi0/oigl_account.


    TRY.
        o_utiles =  NEW zpl_utiles( ).

        lt_claves[] = i_claves[].
        DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
*-----------------------------------------------------------------------------


        LOOP AT o_utiles->gt_period_var ASSIGNING FIELD-SYMBOL(<fs_period_var>).


          wr_fiscper-option = 'EQ'.
          wr_fiscper-sign   = 'I'.


          IF me->g_tipo_actual EQ 'R'.
            wr_fiscper-low = <fs_period_var>-actual.
          ELSE.
            wr_fiscper-low = <fs_period_var>-plan.
          ENDIF.

          IF <fs_period_var>-tipo EQ 'R'.
            APPEND wr_fiscper TO lr_fiscper_re.
          ELSEIF <fs_period_var>-tipo EQ 'P'.
            APPEND wr_fiscper TO lr_fiscper_pre.
          ENDIF.

        ENDLOOP.





* *1) GASTOS a nivel CUENTA CEBE ORDEN - Fuente CEBES (ZPCA_O01N)
*   1.1 Gasto Reales
        IF lr_fiscper_re[] IS NOT INITIAL.
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
        ENDIF.

*   1.2 Gasto Presupuestado
        IF lr_fiscper_pre[] IS NOT INITIAL.
          SELECT *
          INTO TABLE @DATA(lt_gastos_pre)
          FROM zpl_gastos_pre_var AS a
          FOR ALL ENTRIES IN @lt_claves
          WHERE a~comp_code EQ @lt_claves-comp_code
          AND   a~fiscper IN @lr_fiscper_pre
          AND   a~gl_account = @lt_claves-gl_account
          AND   a~costcenter = @lt_claves-costcenter
          AND   a~coorder    = @lt_claves-coorder
          AND   a~/bic/ztabla = @me->c_plan_actual.

          IF sy-subrc EQ 0.
            SORT lt_gastos_pre BY comp_code fiscper gl_account costcenter coorder.
          ENDIF.
        ENDIF.






**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************
        LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


          CLEAR: wa_result.

*     Inicializa wa_result:
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-/bic/ztabla = me->c_actual.
          wa_result-unit    = 'UN'.
          wa_result-currency = 'PYG'.
          wa_result-version = o_utiles->c_version.




*     Agrega los 12 periodos para cada Clave:
          LOOP AT o_utiles->gt_period_var ASSIGNING FIELD-SYMBOL(<fs_perio>).

            CLEAR:  wa_result-/bic/zpl_gasto.


*----------------------------------------*
****   GASTOS ACTUALES/REALES:
*----------------------------------------*

            CALL METHOD o_utiles->get_actual_period
              EXPORTING
                i_tipo   = 'VAR'
                i_period = <fs_perio>-actual "Periodo de Plan
              IMPORTING
                o_actual = wa_result-fiscper. "Periodo Actual/Real


            IF <fs_perio>-tipo EQ 'R'."Tomando el periodo Actual, al momento de Planeamiento
              READ TABLE  lt_gastos_re ASSIGNING FIELD-SYMBOL(<fs_gasto>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                               fiscper    = wa_result-fiscper
                                                               gl_account = <fs_clave>-gl_account
                                                               costcenter = <fs_clave>-costcenter
                                                               coorder    = <fs_clave>-coorder
                                                           BINARY SEARCH.

              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_gasto = <fs_gasto>-gastos * 100.
              ENDIF.




            ELSEIF <fs_perio>-tipo EQ 'P'. "Utiliza el Presupuesto Año anterior como Real

              READ TABLE  lt_gastos_pre ASSIGNING FIELD-SYMBOL(<fs_gastopre>)  WITH KEY comp_code  = <fs_clave>-comp_code
                                                               fiscper    = wa_result-fiscper
                                                               gl_account = <fs_clave>-gl_account
                                                               costcenter = <fs_clave>-costcenter
                                                               coorder    = <fs_clave>-coorder
                                                           BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_gasto = <fs_gastopre>-gastos * 100.
              ENDIF.


            ENDIF.


            wa_result-/bic/zpl_gasto = abs( wa_result-/bic/zpl_gasto  ).
            wa_result-fiscyear = wa_result-fiscper(4).


*------------------------------------


            APPEND wa_result TO lt_aux.

          ENDLOOP." Periodos
        ENDLOOP." Claves Source Package

        e_result = lt_aux[].




      CATCH  cx_root INTO DATA(l_excepcion).
    ENDTRY.
  ENDMETHOD.


  METHOD get_var_init.

    DATA: o_utiles TYPE REF TO zpl_utiles.
    DATA: l_coef TYPE p DECIMALS 4.
    DATA: lr_fiscper_re  TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_pre TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper     LIKE LINE OF lr_fiscper_re.

    DATA: wa_result      TYPE ty_wa_result_var,
          lt_claves      TYPE ty_lt_claves, "ordenada
          lt_clave_venta TYPE ty_lt_clave_venta,
          lt_aux         TYPE ty_lt_result_var.


    lt_claves[] = i_claves[].
    DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.


    CHECK lt_claves[] IS NOT INITIAL.

******** Recorre las Claves del SOURCE_PACKAGE
    LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


*     Agrega los 12 periodos para cada Clave:
      LOOP AT me->gt_period_var ASSIGNING FIELD-SYMBOL(<fs_perio>).

        CLEAR wa_result.
*
***     Inicializa wa_result:
        MOVE-CORRESPONDING <fs_clave> TO wa_result.
        wa_result-chrt_accts = 'SQS'.
        wa_result-fiscvarnt  = 'K4'.
        wa_result-unit    = 'UN'.
        wa_result-currency = 'PYG'.
        wa_result-version = me->g_draft_version.
* Ratios
        wa_result-/bic/zpl_coef = 0. "Gasto Real/Venta REal
        wa_result-/bic/zpl_gasto = 0."Gasto REal
        wa_result-/bic/zpl_gastp = 0."Gasto Presupuestado (auxiliar)
        wa_result-/bic/zpl_part = 0."% Participacion sobre la Venta Presupuestada
        wa_result-/bic/zpl_venta = 0."Venta REal
        wa_result-/bic/zpl_ventp = 0."Venta Presupuestada - SE ACTUALIZA ASINCRONICAMENTE
        wa_result-/bic/zpl_cofin = 0."% Incremento
        wa_result-/bic/zquantity = 0."Cantidad Vendida


        CALL METHOD me->get_tabla( IMPORTING o_tabla = wa_result-/bic/ztabla ).


        CALL METHOD me->get_resp(
          EXPORTING
            i_soc    = wa_result-comp_code
            i_soco   = wa_result-co_area
            i_cuenta = wa_result-gl_account
            i_cebe   = wa_result-costcenter
            i_orden  = wa_result-coorder
          IMPORTING
            o_user   = wa_result-username
                       ).


        CALL METHOD me->get_plan_period
          EXPORTING
            i_tipo   = 'VAR'
            i_period = <fs_perio>-actual "Periodo de input/precarga
          IMPORTING
            o_plan   = wa_result-fiscper. "Periodo Plan


        wa_result-fiscyear = wa_result-fiscper(4).



        APPEND wa_result TO e_result.
        CLEAR wa_result.

      ENDLOOP." Periodos
    ENDLOOP." Claves Source Package






  ENDMETHOD.


  METHOD get_version.
    o_version = c_version.
  ENDMETHOD.


  METHOD get_version_fore.
    o_version = c_version_fore.
  ENDMETHOD.


  METHOD get_vta_2000.


    DATA:
      wa_result TYPE ty_wa_result_vta,
      lt_aux    TYPE ty_lt_result_vta.

    DATA: o_utiles       TYPE REF TO zpl_utiles,
          l_coef         TYPE p DECIMALS 4,
          lr_fiscper_re  TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_pre TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper     LIKE LINE OF lr_fiscper_re,
*          lt_glaccount   TYPE RANGE OF /bi0/oigl_account,
          lt_claves      TYPE ty_lt_claves, "ordenada
          lt_clave_venta TYPE ty_lt_clave_venta_2000.


    CHECK i_claves[] IS NOT INITIAL.

    TRY.

        o_utiles =  NEW zpl_utiles( ).

        lt_claves[] = i_claves[].
        DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
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


*
*------------------------------------------------------------------------
*2) Obtengo para cada CEBE: Linea, Of de Venta y Marca

*    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
*    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

        SELECT
        b~co_area,
        b~costcenter,
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

          lt_clave_venta = CORRESPONDING #( lt_costcenter ).
          DELETE ADJACENT DUPLICATES FROM lt_clave_venta.
        ENDIF.

*------------------------------------------------------------------------


        IF lt_clave_venta[] IS NOT INITIAL.
*----------------------------------------------
**3) VENTAS REALES Agrupadas por Sucursal, Canal, Linea,  Marca - Fuente Reales CEBE (ZPCA_O01N)
*
**3.0) Busca la Venta Real. utilizando el CECO.
*          SELECT
*              a~comp_code,
*              a~co_area,
*              a~fiscper,
*              a~cebe,
*              a~ventastotal AS ventastotal,
*              a~canttotal   AS canttotal
*           INTO TABLE @DATA(lt_vta_real_ceco)
*           FROM
*            zpl_vta_real_cebe( i_datum = @sy-datum ) AS a
*            FOR ALL ENTRIES IN @lt_clave_venta
*          WHERE a~co_area EQ @lt_clave_venta-co_area
*           AND  a~fiscper IN @lr_fiscper_re
*           AND   a~cebe EQ @lt_clave_venta-costcenter.
*          IF sy-subrc EQ 0.
*            SORT lt_vta_real_ceco BY comp_code fiscper cebe.
*          ENDIF.
*
*
**3.1) Selecciona los Reales para calcular coeficiente de Participacion del Gasto sobre la Venta
*
*
*          SELECT * "Explicito el * para que la modificacion solo deba ser a nivel CDS
*           INTO TABLE @DATA(lt_vta_real_s1)
*           FROM
*            zpl_vta_real_s2_01_sum( i_datum = @sy-datum ) AS a
*            FOR ALL ENTRIES IN @lt_clave_venta
*          WHERE a~co_area EQ @lt_clave_venta-co_area
*          AND   a~fiscper IN @lr_fiscper_re
*          AND   a~sucursal EQ @lt_clave_venta-sales_off
*          AND   a~canal    EQ @lt_clave_venta-distr_chan
*          AND   a~linea EQ @lt_clave_venta-matl_group
*          AND   a~marca    EQ @lt_clave_venta-/bic/zmar.
*
*          IF sy-subrc EQ 0.
*            SORT lt_vta_real_s1 BY comp_code fiscper sucursal canal linea  marca.
*          ENDIF.
*
*
**3.1.2)   Busca las ventas agrupadas por Sucursal, Canal y Línea
*          SELECT *
*          INTO TABLE @DATA(lt_vta_real_s2)
*          FROM
*           zpl_vta_real_s2_02_sum( i_datum = @sy-datum ) AS a
*           FOR ALL ENTRIES IN @lt_clave_venta
*         WHERE a~co_area EQ @lt_clave_venta-co_area
*         AND   a~fiscper IN @lr_fiscper_re
*         AND   a~sucursal EQ @lt_clave_venta-sales_off
*          AND   a~canal    EQ @lt_clave_venta-distr_chan
*          AND   a~linea EQ @lt_clave_venta-matl_group
*         AND   a~ventastotal NE 0.
*
*          IF sy-subrc EQ 0.
*            SORT lt_vta_real_s2 BY comp_code fiscper sucursal canal linea.
*          ENDIF.
*
**3.1.3)   Busca las ventas agrupadas por Sucursal, Canal
*          SELECT *
*            INTO TABLE @DATA(lt_vta_real_s3)
*            FROM
*             zpl_vta_real_s2_03_sum( i_datum = @sy-datum ) AS a
*             FOR ALL ENTRIES IN @lt_clave_venta
*           WHERE a~co_area EQ @lt_clave_venta-co_area
*           AND   a~fiscper IN @lr_fiscper_re
*            AND  a~sucursal EQ @lt_clave_venta-sales_off
*            AND   a~canal    EQ @lt_clave_venta-distr_chan
*            AND  a~ventastotal NE 0.
*          IF sy-subrc EQ 0.
*            SORT lt_vta_real_s3 BY comp_code fiscper sucursal canal.
*          ENDIF.


*
*
**3.1.4)   Busca las ventas agrupadas por Sucursal
*        SELECT *
*        INTO TABLE @DATA(lt_vta_real_s4)
*        FROM
*         zpl_vta_real_s2_04_sum( i_datum = @sy-datum ) AS a
*         FOR ALL ENTRIES IN @lt_clave_venta
*       WHERE a~co_area EQ @lt_clave_venta-co_area
*       AND   a~fiscper IN @lr_fiscper_re
*        AND  a~sucursal EQ @lt_clave_venta-sales_off
*        AND  a~ventastotal NE 0.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_real_s4 BY comp_code fiscper sucursal.
*        ENDIF.
*


*----------------------------------------------
***VENTAS PRESUPUESTADAS (fuente COPA - 23V0 por ejemplo) - ZCOP_A01
*4) Selecciona la venta PRESUPUESTADA para calcular el Gasto Presupuestado = Vta Pres * Coef
          CLEAR lr_fiscper_vta[].
          LOOP AT o_utiles->gt_period_var ASSIGNING <fs_period_var>.

            wr_fiscper-low = <fs_period_var>-plan.
            wr_fiscper-option = 'EQ'.
            wr_fiscper-sign   = 'I'.
            APPEND wr_fiscper TO lr_fiscper_vta.

          ENDLOOP.


*4.0) Busca la Venta Presup. utilizando el CECO.
          SELECT
              a~comp_code,
              a~co_area,
              a~fiscper,
              a~cebe,
              a~ventas AS ventastotal,
              a~cant   AS canttotal
           INTO TABLE @DATA(lt_vta_ceco)
           FROM
            zpl_cebes_pres( p_datum = @sy-datum ) AS a
            FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~co_area EQ @lt_clave_venta-co_area
           AND  a~fiscper IN @lr_fiscper_vta
           AND   a~cebe EQ @lt_clave_venta-costcenter
           AND   a~tipo EQ 'venta'
           AND   a~tabla EQ @me->c_plan.
          IF sy-subrc EQ 0.
            SORT lt_vta_ceco BY comp_code fiscper cebe.
          ENDIF.


*4.1)Clave completa: Sucursal, Canal, Linea, Marca
          SELECT *
         INTO TABLE @DATA(lt_vta_pres_s1)
         FROM
          zpl_vta_pres_s2_01_sum( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @lt_clave_venta
        WHERE a~co_area EQ @lt_clave_venta-co_area
         AND  a~fiscper IN @lr_fiscper_vta
        AND   a~sucursal EQ @lt_clave_venta-sales_off
        AND   a~canal    EQ @lt_clave_venta-distr_chan
        AND   a~linea EQ @lt_clave_venta-matl_group
        AND   a~marca    EQ @lt_clave_venta-/bic/zmar
        AND   a~tipo EQ 'venta'
        AND   a~tabla = @me->c_plan.

          IF sy-subrc EQ 0.
            SORT lt_vta_pres_s1 BY comp_code fiscper sucursal canal linea marca.
          ENDIF.


*4,2) Sucursal, Canal, Linea:S2
          SELECT *
            INTO TABLE @DATA(lt_vta_pres_s2)
            FROM
             zpl_vta_pres_s2_02_sum( i_datum = @sy-datum ) AS a
             FOR ALL ENTRIES IN @lt_clave_venta
           WHERE a~co_area EQ @lt_clave_venta-co_area
           AND   a~fiscper IN @lr_fiscper_vta
           AND   a~sucursal EQ @lt_clave_venta-sales_off
           AND   a~canal    EQ @lt_clave_venta-distr_chan
           AND   a~linea EQ @lt_clave_venta-matl_group
           AND   a~tabla = @me->c_plan.
          IF sy-subrc EQ 0.
            SORT lt_vta_pres_s2 BY comp_code fiscper sucursal canal linea.
          ENDIF.


*4.3) Sucursal, Canal: S3
          SELECT *
            INTO TABLE @DATA(lt_vta_pres_s3)
            FROM
             zpl_vta_pres_s2_03_sum( i_datum = @sy-datum ) AS a
             FOR ALL ENTRIES IN @lt_clave_venta
           WHERE  a~co_area EQ @lt_clave_venta-co_area
           AND   a~fiscper IN @lr_fiscper_vta
           AND   a~sucursal EQ @lt_clave_venta-sales_off
           AND   a~canal    EQ @lt_clave_venta-distr_chan
           AND   a~tabla = @me->c_plan.
          IF sy-subrc EQ 0.
            SORT lt_vta_pres_s3 BY comp_code fiscper sucursal canal.
          ENDIF.

**4.4) Sucursal: S4
*        SELECT *
*          INTO TABLE @DATA(lt_vta_pres_s4)
*          FROM
*           zpl_vta_pres_s2_04_sum( i_datum = @sy-datum ) AS a
*           FOR ALL ENTRIES IN @lt_clave_venta
*         WHERE  a~co_area EQ @lt_clave_venta-co_area
*         AND  a~fiscper IN @lr_fiscper_vta
*         AND   a~sucursal EQ @lt_clave_venta-sales_off.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_pres_s4 BY comp_code fiscper sucursal.
*        ENDIF.


        ENDIF."clave venta no INICIAL




*5) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
*        READ TABLE lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>)  INDEX 1.

*        SELECT 'I' AS sign, 'EQ' AS option, a~gl_account
        SELECT *
       INTO TABLE @DATA(lt_glaccount)
       FROM /bic/azpl_a292 AS a
       FOR ALL ENTRIES IN @lt_claves
       WHERE a~username  EQ @lt_claves-username
       AND   a~comp_code EQ @lt_claves-comp_code
       AND   a~/bic/zpl_dum01 EQ '1'.
        IF sy-subrc EQ 0.
          SORT lt_glaccount BY comp_code username gl_account.
        ENDIF.




**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************
        LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


******* Agrupadores Sucursal, LInea Marca
          READ TABLE lt_costcenter ASSIGNING FIELD-SYMBOL(<fs_cost>) WITH KEY costcenter = <fs_clave>-costcenter.
          CHECK sy-subrc EQ 0.

          CLEAR: l_coef,  wa_result.

*     Inicializa wa_result:
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          MOVE-CORRESPONDING <fs_cost> TO wa_result.
          wa_result-costcenter = <fs_cost>-costcenter.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-/bic/ztabla = me->c_plan.
          wa_result-version = me->c_version.
          wa_result-unit    = me->c_unit.
          wa_result-currency = me->c_pyg.




*     Agrega los 12 periodos para cada Clave:
          LOOP AT o_utiles->gt_period_var ASSIGNING FIELD-SYMBOL(<fs_perio>).

            CLEAR: wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp, wa_result-/bic/zpl_ventq, wa_result-/bic/zpl_vtaqp.




*----------------------------------------*
* VENTA RESUPUESTADA (25V0)
*----------------------------------------*
            READ TABLE lt_vta_ceco ASSIGNING FIELD-SYMBOL(<fs_vtarealceco>) WITH KEY
                                                                       comp_code = <fs_clave>-comp_code
                                                                       fiscper   = <fs_perio>-plan
                                                                       cebe      = <fs_clave>-costcenter
                                                                       BINARY SEARCH.
            IF sy-subrc EQ 0.
              wa_result-/bic/zpl_ventp =  abs( <fs_vtarealceco>-ventastotal ).
              wa_result-/bic/zpl_vtaqp = abs( <fs_vtarealceco>-canttotal ).
            ELSE.


              READ TABLE lt_vta_pres_s1 ASSIGNING FIELD-SYMBOL(<fs_venta_p1>)  WITH KEY  comp_code = <fs_clave>-comp_code
                                                                                    fiscper   = <fs_perio>-plan
                                                                                    sucursal  = <fs_cost>-sales_off
                                                                                    canal     = <fs_cost>-distr_chan
                                                                                    linea     = <fs_cost>-matl_group
                                                                                    marca     = <fs_cost>-/bic/zmar
                                                                                    BINARY SEARCH.

              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_ventp =  abs( <fs_venta_p1>-ventastotal ).
                wa_result-/bic/zpl_vtaqp = abs( <fs_venta_p1>-canttotal ).

              ELSE.
*          Busca las ventas agrupadas por  Sucursal, Canal, Linea
                READ TABLE lt_vta_pres_s2 ASSIGNING FIELD-SYMBOL(<fs_venta_p2>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                              fiscper   = <fs_perio>-plan
                                                                             sucursal  = <fs_cost>-sales_off
                                                                             canal     = <fs_cost>-distr_chan
                                                                             linea     = <fs_cost>-matl_group
                                                                             BINARY SEARCH.
                IF sy-subrc EQ 0.
                  wa_result-/bic/zpl_ventp = abs( <fs_venta_p2>-ventastotal ).
                  wa_result-/bic/zpl_vtaqp = abs( <fs_venta_p2>-canttotal ).
                ELSE.
*            Busca las ventas agrupadas por sucursal, canal
                  READ TABLE lt_vta_pres_s3 ASSIGNING FIELD-SYMBOL(<fs_venta_p3>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-plan
                                                                               sucursal  = <fs_cost>-sales_off
                                                                               canal     = <fs_cost>-distr_chan
                                                                               BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    wa_result-/bic/zpl_ventp = abs( <fs_venta_p3>-ventastotal ).
                    wa_result-/bic/zpl_vtaqp = abs( <fs_venta_p3>-canttotal ).

                  ELSE.
*            Busca las ventas agrupadas por sucursal
*                  READ TABLE lt_vta_pres_s4 ASSIGNING FIELD-SYMBOL(<fs_venta_p4>)  WITH KEY comp_code = <fs_clave>-comp_code
*                                                                               fiscper   = <fs_perio>-plan
*                                                                               sucursal  = <fs_cost>-sales_off
*                                                                               BINARY SEARCH.
*                  IF sy-subrc EQ 0.
*                    READ TABLE lt_glaccount WITH KEY comp_code   = <fs_clave>-comp_code
*                                                     username    = <fs_clave>-username
*                                                     gl_account  = <fs_clave>-gl_account
*                                                     BINARY SEARCH TRANSPORTING NO FIELDS.
*                    IF sy-subrc NE 0. "La cuenta no es Excepción, entonces llega a nivel Oficina de Ventas
*                      wa_result-/bic/zpl_ventp = abs(  <fs_venta_p4>-ventastotal * 100 ).
*                      wa_result-/bic/zpl_vtaqp = abs( <fs_venta_p4>-canttotal ).
*                    ENDIF.
*                  ENDIF."Sucursal
                  ENDIF."Sucursal, Canal
                ENDIF."Sucursal, Canal, LInea
              ENDIF."Sucursal, Linea Marca
            ENDIF."CECO


*--------------------------------------------------------

            CALL METHOD o_utiles->get_plan_period
              EXPORTING
                i_tipo   = 'VAR'
                i_period = <fs_perio>-actual "Periodo de input/precarga
              IMPORTING
                o_plan   = wa_result-fiscper. "Periodo Plan


            wa_result-fiscyear = wa_result-fiscper(4).

            APPEND wa_result TO lt_aux.

          ENDLOOP." Periodos
        ENDLOOP." Claves Source Package

        e_result = lt_aux[].

      CATCH  cx_root INTO DATA(l_excepcion).

    ENDTRY.



  ENDMETHOD.


  METHOD get_vta_actual.


*********************************************************************************************
    DATA: o_utiles TYPE REF TO zpl_utiles.
    DATA: l_coef TYPE p DECIMALS 4.
    DATA: lr_fiscper_re  TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_pre TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper     LIKE LINE OF lr_fiscper_re.

    DATA: wa_result      TYPE ty_wa_result_vta,
          lt_claves      TYPE ty_lt_claves, "ordenada
          lt_clave_venta TYPE ty_lt_clave_venta,
          lt_aux         TYPE ty_lt_result_vta,
          lt_glaccount   TYPE RANGE OF /bi0/oigl_account.


    TRY.
        o_utiles =  NEW zpl_utiles( ).

        lt_claves[] = i_claves[].
        DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
*-----------------------------------------------------------------------------


        LOOP AT o_utiles->gt_period_vta ASSIGNING FIELD-SYMBOL(<fs_period_var>).

          wr_fiscper-low = <fs_period_var>-actual.
          wr_fiscper-option = 'EQ'.
          wr_fiscper-sign   = 'I'.

          IF <fs_period_var>-tipo EQ 'R'.
            APPEND wr_fiscper TO lr_fiscper_re.
          ELSE.
            APPEND wr_fiscper TO lr_fiscper_pre.
          ENDIF.
        ENDLOOP.




*------------------------------------------------------------------------
*1) Obtengo para cada CEBE: Linea, Of de Venta y Marca

*    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
*    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

        SELECT
        b~co_area,
        b~costcenter,
        b~sales_off,
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
          DELETE lt_costcenter WHERE sales_off EQ '' AND matl_group EQ '' AND /bic/zmar EQ ''.

          lt_clave_venta = CORRESPONDING #( lt_costcenter ).
          DELETE ADJACENT DUPLICATES FROM lt_clave_venta.
        ENDIF.

*------------------------------------------------------------------------

*3) VENTAS REALES Agrupadas por Linea, Of Vta y Marca - Fuente Reales CEBE (ZPCA_O01N)
*3.1) Selecciona los Reales para calcular coeficiente de Participacion del Gasto sobre la Venta


        SELECT * "Explicito el * para que la modificacion solo deba ser a nivel CDS
         INTO TABLE @DATA(lt_ventas_re)
         FROM
          zpl_cebes_real( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @lt_clave_venta
        WHERE a~co_area EQ @lt_clave_venta-co_area
        AND   a~fiscper IN @lr_fiscper_re
        AND   a~sucursal EQ @lt_clave_venta-sales_off
        AND   a~linea EQ @lt_clave_venta-matl_group
        AND   a~marca    EQ @lt_clave_venta-/bic/zmar
        .

        IF sy-subrc EQ 0.
          SORT lt_ventas_re BY comp_code fiscper sucursal linea marca.
        ENDIF.


*3.1.2)   Busca las ventas agrupadas por Sucursal y Línea
        SELECT *
        INTO TABLE @DATA(lt_ventas_suc_lin)
        FROM
         zpl_vta_suc_linea( i_datum = @sy-datum ) AS a
         FOR ALL ENTRIES IN @lt_clave_venta
       WHERE a~co_area EQ @lt_clave_venta-co_area
       AND   a~fiscper IN @lr_fiscper_re
       AND   a~sucursal EQ @lt_clave_venta-sales_off
       AND   a~linea EQ @lt_clave_venta-matl_group
       AND   a~ventastotal NE 0.

        IF sy-subrc EQ 0.

          SORT lt_ventas_suc_lin BY comp_code fiscper sucursal linea.
        ENDIF.

*3.1.3)   Busca las ventas agrupadas por Sucursal
        SELECT *
        INTO TABLE @DATA(lt_ventas_suc_)
        FROM
         zpl_vta_suc_( i_datum = @sy-datum ) AS a
         FOR ALL ENTRIES IN @lt_clave_venta
       WHERE a~co_area EQ @lt_clave_venta-co_area
       AND   a~fiscper IN @lr_fiscper_re
        AND  a~sucursal EQ @lt_clave_venta-sales_off
        AND  a~ventastotal NE 0.
        IF sy-subrc EQ 0.
          SORT lt_ventas_suc_ BY comp_code fiscper sucursal.
        ENDIF.

*------------------------------------------------------------------------






*------------------------------------------------------------------------
***VENTAS PRESUPUESTADAS (fuente COPA - 24V0 por ejemplo) - ZCOP_A01
*4) Selecciona la venta PRESUPUESTADA para calcular el Gasto Presupuestado = Vta Pres * Coef
*   Además puede incluir Venta Presupuestada del año en curso en vez de Reales
        CLEAR lr_fiscper_vta[].
        LOOP AT o_utiles->gt_period_vta ASSIGNING <fs_period_var>.

          wr_fiscper-low = <fs_period_var>-plan.
          wr_fiscper-option = 'EQ'.
          wr_fiscper-sign   = 'I'.
          APPEND wr_fiscper TO lr_fiscper_vta.

        ENDLOOP.

        IF NOT lr_fiscper_pre[] IS INITIAL.
          APPEND LINES OF lr_fiscper_pre TO lr_fiscper_vta.
        ENDIF.

        SELECT
          a~comp_code,
          a~co_area,
          a~fiscper,
          a~linea,
          a~sucursal,
          a~marca,
          a~gastostotal,
          a~ventastotal,
          a~canttotal
       INTO TABLE @DATA(lt_vta_pres)
       FROM
        zpl_cebes_pres_u( i_datum = @sy-datum ) AS a
        FOR ALL ENTRIES IN @lt_clave_venta
      WHERE a~co_area EQ @lt_clave_venta-co_area
       AND  a~fiscper IN @lr_fiscper_vta
       AND   a~sucursal EQ @lt_clave_venta-sales_off
      AND   a~linea EQ @lt_clave_venta-matl_group
      AND   a~marca    EQ @lt_clave_venta-/bic/zmar
      AND   a~tipo EQ 'venta'.
        IF sy-subrc EQ 0.
          SORT lt_vta_pres BY comp_code fiscper sucursal linea marca.
        ENDIF.


        SELECT *
          INTO TABLE @DATA(lt_vta_pres_lin_suc)
          FROM
           zpl_vtas_pres_suc_linea( i_datum = @sy-datum ) AS a
           FOR ALL ENTRIES IN @lt_clave_venta
         WHERE a~co_area EQ @lt_clave_venta-co_area
          AND  a~fiscper IN @lr_fiscper_vta
         AND   a~sucursal EQ @lt_clave_venta-sales_off
         AND   a~linea EQ @lt_clave_venta-matl_group.
        IF sy-subrc EQ 0.
          SORT lt_vta_pres_lin_suc BY comp_code fiscper sucursal linea .
        ENDIF.



        SELECT *
          INTO TABLE @DATA(lt_vta_pres_suc)
          FROM
           zpl_vtas_pres_suc_( i_datum = @sy-datum ) AS a
           FOR ALL ENTRIES IN @lt_clave_venta
         WHERE  a~co_area EQ @lt_clave_venta-co_area
         AND  a~fiscper IN @lr_fiscper_vta
         AND   a~sucursal EQ @lt_clave_venta-sales_off.
        IF sy-subrc EQ 0.
          SORT lt_vta_pres_suc BY comp_code fiscper sucursal.
        ENDIF.
*------------------------------------------------------------------------






*5) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
        READ TABLE lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>)  INDEX 1.
        IF sy-subrc EQ 0.

          SELECT 'I' AS sign, 'EQ' AS option, a~gl_account
          INTO TABLE @lt_glaccount
          FROM /bic/azpl_a292 AS a
          WHERE a~username EQ @<fs_clave>-username
          AND   a~comp_code EQ @<fs_clave>-comp_code
          AND   a~/bic/zpl_dum01 EQ '1'.

        ENDIF.


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
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-/bic/ztabla = me->c_actual.
          wa_result-unit    = 'UN'.
          wa_result-currency = 'PYG'.
          wa_result-version = me->c_version.



*     Agrega los 12 periodos para cada Clave:
          LOOP AT me->gt_period_vta ASSIGNING FIELD-SYMBOL(<fs_perio>).

            CLEAR:  wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp, wa_result-/bic/zpl_ventq,
                    wa_result-/bic/zpl_vtaqp.


*----------------------------------------*
****   PERIODOS REALES:
*----------------------------------------*

            IF <fs_perio>-tipo EQ 'R'.



********VENTAS REALES:
              READ TABLE lt_ventas_re ASSIGNING FIELD-SYMBOL(<fs_venta>) WITH KEY  comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_cost>-sales_off
                                                                               linea     = <fs_cost>-matl_group
                                                                               marca     = <fs_cost>-/bic/zmar
                                                                               BINARY SEARCH.

              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_venta = <fs_venta>-ventastotal * 100.
                wa_result-/bic/zpl_ventq = <fs_venta>-canttotal.

              ELSE.
*          Busca las ventas agrupadas por Linea y Sucursal
                READ TABLE lt_ventas_suc_lin ASSIGNING FIELD-SYMBOL(<fs_suc_lin>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                             fiscper   = <fs_perio>-actual
                                                                             sucursal  = <fs_cost>-sales_off
                                                                             linea     = <fs_cost>-matl_group
                                                                             BINARY SEARCH.
                IF sy-subrc EQ 0.
                  wa_result-/bic/zpl_venta = <fs_suc_lin>-ventastotal * 100.
                  wa_result-/bic/zpl_ventq = <fs_venta>-canttotal.
                ELSE.
*            Busca las ventas agrupadas por sucursal
                  READ TABLE lt_ventas_suc_ ASSIGNING FIELD-SYMBOL(<fs_suc_>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-actual
                                                                               sucursal  = <fs_cost>-sales_off
                                                                               BINARY SEARCH.
                  IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN lt_glaccount.
                    wa_result-/bic/zpl_venta = <fs_suc_>-ventastotal * 100.
                    wa_result-/bic/zpl_ventq = <fs_venta>-canttotal.
                  ENDIF."Sucursal
                ENDIF."Linea, Sucursal
              ENDIF."Sucursal, Linea Marca



            ELSE. "Parte Presupuesto, 23V0 por ejemplo
*----------------------------------------*
* PERIODOS RESUPUESTO
*----------------------------------------*
*
*Venta COPA Presupuesto 23V0
*              READ TABLE lt_vta_pres ASSIGNING FIELD-SYMBOL(<fs_venta_pre>) WITH KEY  comp_code = <fs_clave>-comp_code
*                                                                               fiscper   = <fs_perio>-actual
*                                                                               sucursal  = <fs_cost>-sales_off
*                                                                               linea     = <fs_cost>-matl_group
*                                                                               marca     = <fs_cost>-/bic/zmar
*                                                                               BINARY SEARCH.
*
*              IF sy-subrc EQ 0.
*                wa_result-/bic/zpl_ventp = <fs_venta_pre>-ventastotal * 100.
*                wa_result-/bic/zpl_vtaqp = <fs_venta>-canttotal.
*
*              ELSE.
**          Busca las ventas agrupadas por Linea y Sucursal
*                READ TABLE lt_vta_pres_lin_suc ASSIGNING FIELD-SYMBOL(<fs_suc_lin_pre>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                             fiscper   = <fs_perio>-actual
*                                                                             sucursal  = <fs_cost>-sales_off
*                                                                             linea     = <fs_cost>-matl_group
*                                                                             BINARY SEARCH.
*                IF sy-subrc EQ 0.
*                  wa_result-/bic/zpl_ventp = <fs_venta_pre>-ventastotal * 100.
*                  wa_result-/bic/zpl_vtaqp = <fs_venta>-canttotal.
*
*                ELSE.
**            Busca las ventas agrupadas por sucursal
*                  READ TABLE lt_vta_pres_suc ASSIGNING FIELD-SYMBOL(<fs_suc_pre>) WITH KEY comp_code = <fs_clave>-comp_code
*                                                                               fiscper   = <fs_perio>-actual
*                                                                               sucursal  = <fs_cost>-sales_off
*                                                                               BINARY SEARCH.
*                  IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN lt_glaccount.
*                    wa_result-/bic/zpl_ventp = <fs_venta_pre>-ventastotal * 100.
*                    wa_result-/bic/zpl_vtaqp = <fs_venta>-canttotal.
*
*                  ENDIF."Sucursal
*                ENDIF."Linea, Sucursal
*              ENDIF."Sucursal, Linea Marca

            ENDIF."REAL/PRESUP



*----------------------------------------*
* VENTA RESUPUESTADA (24V0)
*----------------------------------------*
            READ TABLE lt_vta_pres ASSIGNING FIELD-SYMBOL(<fs_presup>) WITH KEY
                                                                           comp_code = <fs_clave>-comp_code
                                                                           fiscper   = <fs_perio>-plan
                                                                           sucursal  = <fs_cost>-sales_off
                                                                           linea     = <fs_cost>-matl_group
                                                                           marca     = <fs_cost>-/bic/zmar BINARY SEARCH.
            IF sy-subrc EQ 0.

              wa_result-/bic/zpl_ventp =  abs( <fs_presup>-ventastotal * 100 ).
              wa_result-/bic/zpl_vtaqp =  abs( <fs_presup>-canttotal ).
            ELSE.

*          Busca las ventas agrupadas por Linea y Sucursal
              READ TABLE lt_vta_pres_lin_suc ASSIGNING FIELD-SYMBOL(<fs_suc_lin_pre>) WITH KEY comp_code = <fs_clave>-comp_code
                                                                           fiscper   = <fs_perio>-plan
                                                                           sucursal  = <fs_cost>-sales_off
                                                                           linea     = <fs_cost>-matl_group
                                                                           BINARY SEARCH.
              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_ventp = abs( <fs_suc_lin_pre>-ventastotal * 100 ).
                wa_result-/bic/zpl_vtaqp = <fs_suc_lin_pre>-canttotal.
              ELSE.
*            Busca las ventas agrupadas por sucursal
                READ TABLE lt_vta_pres_suc ASSIGNING FIELD-SYMBOL(<fs_suc_pre>)  WITH KEY comp_code = <fs_clave>-comp_code
                                                                             fiscper   = <fs_perio>-plan
                                                                             sucursal  = <fs_cost>-sales_off
                                                                             BINARY SEARCH.
                IF sy-subrc EQ 0 AND <fs_clave>-gl_account NOT IN lt_glaccount.
                  wa_result-/bic/zpl_ventp = abs( <fs_suc_pre>-ventastotal * 100 ).
                  wa_result-/bic/zpl_vtaqp = abs( <fs_suc_pre>-canttotal ).

                ENDIF."Sucursal
              ENDIF."Linea, Sucursal
            ENDIF."Sucursal, Linea Marca

            wa_result-/bic/zpl_ventp = abs( wa_result-/bic/zpl_ventp  ).
            wa_result-/bic/zpl_vtaqp = abs( wa_result-/bic/zpl_vtaqp ).


*------------------------------------
            wa_result-fiscper  = <fs_perio>-actual.
            wa_result-fiscyear = wa_result-fiscper(4).

            APPEND wa_result TO lt_aux.

          ENDLOOP." Periodos
        ENDLOOP." Claves Source Package

        e_result = lt_aux[].




      CATCH  cx_root INTO DATA(l_excepcion).
    ENDTRY.



  ENDMETHOD.


  METHOD get_vta_forecast.




*********************************************************************************************
    DATA: o_utiles TYPE REF TO zpl_utiles.
    DATA: l_coef TYPE p DECIMALS 4.
    DATA: lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper     LIKE LINE OF lr_fiscper_vta.

    DATA: wa_result      TYPE ty_wa_result_vta,
          lt_claves      TYPE ty_lt_claves, "ordenada
          lt_clave_venta TYPE ty_lt_clave_venta,
          lt_aux         TYPE ty_lt_result_vta.



    TRY.
        o_utiles =  NEW zpl_utiles( ).

        CHECK i_claves[] IS NOT INITIAL.

        lt_claves[] = i_claves[].
        DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
*-----------------------------------------------------------------------------


*        LOOP AT o_utiles->gt_period_vta ASSIGNING FIELD-SYMBOL(<fs_period_var>).
*
*          wr_fiscper-low = <fs_period_var>-actual.
*          wr_fiscper-option = 'EQ'.
*          wr_fiscper-sign   = 'I'.
*
*          IF <fs_period_var>-tipo EQ 'R'.
*            APPEND wr_fiscper TO lr_fiscper_re.
*          ELSE.
*            APPEND wr_fiscper TO lr_fiscper_pre.
*          ENDIF.
*        ENDLOOP.




*------------------------------------------------------------------------
*1) Obtengo para cada CEBE: Linea, Of de Venta y Marca

*    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
*    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

        SELECT
        b~co_area,
        b~costcenter,
        b~sales_off,
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
          DELETE lt_costcenter WHERE sales_off EQ '' AND matl_group EQ '' AND /bic/zmar EQ ''.

          lt_clave_venta = CORRESPONDING #( lt_costcenter ).
          DELETE ADJACENT DUPLICATES FROM lt_clave_venta.
        ENDIF.



*------------------------------------------------------------------------
***VENTAS FORECAST (fuente IBM - Verios 001, 002, etc) - ZCOP_A03

*2) Selecciona la venta FORECAST para calcular el Gasto Forecast = ( Vta Forcast % Part) * Coef

        CLEAR lr_fiscper_vta[].
        LOOP AT o_utiles->gt_period_vta ASSIGNING FIELD-SYMBOL(<fs_period_var>).

          wr_fiscper-low = <fs_period_var>-plan.
          wr_fiscper-option = 'EQ'.
          wr_fiscper-sign   = 'I'.
          APPEND wr_fiscper TO lr_fiscper_vta.

        ENDLOOP.


*2.1) Busca Vta por CECO
        SELECT a~version, a~comp_code, a~co_area, a~cebe AS ceco, fiscper, cant AS canttotal, ventas AS ventastotal
        INTO TABLE @DATA(lt_vta_ceco)
        FROM zpl_vtas_for_01( p_datum = @sy-datum ) AS a
        FOR ALL ENTRIES IN @lt_clave_venta
        WHERE a~version EQ @i_version
        AND   a~co_area   EQ @lt_clave_venta-co_area
        AND   a~cebe      EQ @lt_clave_venta-costcenter.
        IF sy-subrc EQ 0.
          SORT lt_vta_ceco BY version comp_code fiscper ceco.
        ENDIF.

*2.2) Busca por Sucursal, Linea y Marca
        SELECT *
       INTO TABLE @DATA(lt_vta_pres)
       FROM
        zpl_vtas_for_00_sum( i_datum = @sy-datum ) AS a
        FOR ALL ENTRIES IN @lt_clave_venta
      WHERE a~version EQ @i_version
       AND  a~co_area EQ @lt_clave_venta-co_area
       AND  a~fiscper IN @lr_fiscper_vta
       AND   a~sucursal EQ @lt_clave_venta-sales_off
      AND   a~linea EQ @lt_clave_venta-matl_group
      AND   a~marca    EQ @lt_clave_venta-/bic/zmar
      AND   a~tipo EQ 'venta'.
        IF sy-subrc EQ 0.
          SORT lt_vta_pres BY version comp_code fiscper sucursal linea marca
*          linea
          marca.
        ENDIF.
*
**2.3) Busca por Sucursal y Linea:
*        SELECT *
*          INTO TABLE @DATA(lt_vta_pres_lin_suc)
*          FROM
*           zpl_vtas_for_02_sum( i_datum = @sy-datum ) AS a
*           FOR ALL ENTRIES IN @lt_clave_venta
*         WHERE a~version EQ @i_version
*       AND  a~co_area EQ @lt_clave_venta-co_area
*          AND  a~fiscper IN @lr_fiscper_vta
*         AND   a~sucursal EQ @lt_clave_venta-sales_off
*         AND   a~linea EQ @lt_clave_venta-matl_group.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_pres_lin_suc BY version comp_code fiscper sucursal linea .
*        ENDIF.

*10/5/2024: Corta la búsqueda en Sucursal y Linea
*---------------------------------------------------------------
*
*        SELECT *
*          INTO TABLE @DATA(lt_vta_pres_suc)
*          FROM
*           zpl_vtas_for_03_sum( i_datum = @sy-datum ) AS a
*           FOR ALL ENTRIES IN @lt_clave_venta
*         WHERE a~version EQ @i_version
*       AND  a~co_area EQ @lt_clave_venta-co_area
*         AND  a~fiscper IN @lr_fiscper_vta
*         AND   a~sucursal EQ @lt_clave_venta-sales_off.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_pres_suc BY version comp_code fiscper sucursal.
*        ENDIF.
*------------------------------------------------------------------------






*5) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
        SELECT *
       INTO TABLE @DATA(lt_glaccount)
       FROM /bic/azpl_a292 AS a
       FOR ALL ENTRIES IN @lt_claves
       WHERE a~username  EQ @lt_claves-username
       AND   a~comp_code EQ @lt_claves-comp_code
       AND   a~/bic/zpl_dum01 EQ '1'.
        IF sy-subrc EQ 0.
          SORT lt_glaccount BY comp_code username gl_account.
        ENDIF.


**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************
        LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


******* Agrupadores Sucursal, LInea Marca
          READ TABLE lt_costcenter ASSIGNING FIELD-SYMBOL(<fs_cost>) WITH KEY costcenter = <fs_clave>-costcenter.
          CHECK sy-subrc EQ 0.

          CLEAR: l_coef,  wa_result.

*     Inicializa wa_result:
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          MOVE-CORRESPONDING <fs_cost> TO wa_result.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-/bic/ztabla = me->c_forecast.
          wa_result-unit    = 'UN'.
          wa_result-currency = 'PYG'.
          wa_result-version = i_version.



*     Agrega los 12 periodos para cada Clave:
          LOOP AT me->gt_period_vta ASSIGNING FIELD-SYMBOL(<fs_perio>).

            CLEAR:  wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp, wa_result-/bic/zpl_ventq,
                    wa_result-/bic/zpl_vtaqp.


*----------------------------------------*
* VENTA FORECAST
*----------------------------------------*
            READ TABLE lt_vta_ceco ASSIGNING FIELD-SYMBOL(<fs_vta_ceco>) WITH KEY
                                                                           version  = i_version
                                                                           comp_code = <fs_clave>-comp_code
                                                                           fiscper   = <fs_perio>-plan
                                                                           ceco      = <fs_clave>-costcenter BINARY SEARCH.
            IF sy-subrc EQ 0.
              wa_result-/bic/zpl_ventp =  abs( <fs_vta_ceco>-ventastotal ).
              wa_result-/bic/zpl_vtaqp =  abs( <fs_vta_ceco>-canttotal ).


            ELSE.


              READ TABLE lt_vta_pres ASSIGNING FIELD-SYMBOL(<fs_presup>) WITH KEY
                                                                             version  = i_version
                                                                             comp_code = <fs_clave>-comp_code
                                                                             fiscper   = <fs_perio>-plan
                                                                             sucursal  = <fs_cost>-sales_off
                                                                             linea     = <fs_cost>-matl_group
                                                                             marca     = <fs_cost>-/bic/zmar BINARY SEARCH.
              IF sy-subrc EQ 0.

                wa_result-/bic/zpl_ventp =  abs( <fs_presup>-ventastotal ).
                wa_result-/bic/zpl_vtaqp =  abs( <fs_presup>-canttotal ).
              ELSE.

*          Busca las ventas agrupadas por Linea y Sucursal
*                READ TABLE lt_vta_pres_lin_suc ASSIGNING FIELD-SYMBOL(<fs_suc_lin_pre>) WITH KEY
*                                                                             version  = i_version
*                                                                             comp_code = <fs_clave>-comp_code
*                                                                             fiscper   = <fs_perio>-plan
*                                                                             sucursal  = <fs_cost>-sales_off
*                                                                             linea     = <fs_cost>-matl_group
*                                                                             BINARY SEARCH.
*                IF sy-subrc EQ 0.
*                  wa_result-/bic/zpl_ventp = abs( <fs_suc_lin_pre>-ventastotal ).
*                  wa_result-/bic/zpl_vtaqp = <fs_suc_lin_pre>-canttotal.
*                ELSE.
*            Busca las ventas agrupadas por sucursal
*                READ TABLE lt_vta_pres_suc ASSIGNING FIELD-SYMBOL(<fs_suc_pre>)  WITH KEY
*                                                                                 version  = o_utiles->c_forecast
*                                                                                comp_code = <fs_clave>-comp_code
*                                                                             fiscper   = <fs_perio>-plan
*                                                                             sucursal  = <fs_cost>-sales_off
*                                                                             BINARY SEARCH.
*                IF sy-subrc EQ 0.
*                  READ TABLE lt_glaccount WITH KEY    comp_code   = <fs_clave>-comp_code
*                                                      username    = <fs_clave>-username
*                                                      gl_account  = <fs_clave>-gl_account
*                                                      BINARY SEARCH TRANSPORTING NO FIELDS.
*                  IF sy-subrc NE 0. "La cuenta no es Excepción, entonces llega a nivel Oficina de Ventas
*                    wa_result-/bic/zpl_ventp = abs( <fs_suc_pre>-ventastotal ).
*                    wa_result-/bic/zpl_vtaqp = abs( <fs_suc_pre>-canttotal ).
              ENDIF."CECO
            ENDIF."Sucursal y Marca
*            ENDIF."Linea, Sucursal
*            ENDIF."Sucursal, Linea Marca

            wa_result-/bic/zpl_ventp = abs( wa_result-/bic/zpl_ventp  ).
            wa_result-/bic/zpl_vtaqp = abs( wa_result-/bic/zpl_vtaqp ).


*------------------------------------
            wa_result-fiscper  = <fs_perio>-plan.
            wa_result-fiscyear = wa_result-fiscper(4).

            APPEND wa_result TO lt_aux.

          ENDLOOP." Periodos
        ENDLOOP." Claves Source Package

        e_result = lt_aux[].




      CATCH  cx_root INTO DATA(l_excepcion).
    ENDTRY.




  ENDMETHOD.


  METHOD get_vta_fore_1000.




*********************************************************************************************
    DATA: o_utiles TYPE REF TO zpl_utiles.
    DATA: l_coef TYPE p DECIMALS 4.
    DATA: lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper     LIKE LINE OF lr_fiscper_vta.

    DATA: wa_result      TYPE ty_wa_result_vta,
          lt_claves      TYPE ty_lt_claves, "ordenada
          lt_clave_venta TYPE ty_lt_clave_venta,
          lt_aux         TYPE ty_lt_result_vta.



    TRY.
        o_utiles =  NEW zpl_utiles( ).

        CHECK i_claves[] IS NOT INITIAL.

        lt_claves[] = i_claves[].
        DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
*-----------------------------------------------------------------------------


*        LOOP AT o_utiles->gt_period_vta ASSIGNING FIELD-SYMBOL(<fs_period_var>).
*
*          wr_fiscper-low = <fs_period_var>-actual.
*          wr_fiscper-option = 'EQ'.
*          wr_fiscper-sign   = 'I'.
*
*          IF <fs_period_var>-tipo EQ 'R'.
*            APPEND wr_fiscper TO lr_fiscper_re.
*          ELSE.
*            APPEND wr_fiscper TO lr_fiscper_pre.
*          ENDIF.
*        ENDLOOP.




*------------------------------------------------------------------------
*1) Obtengo para cada CEBE: Linea, Of de Venta y Marca

*    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
*    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

        SELECT
        b~co_area,
        b~costcenter,
        b~sales_off,
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
          DELETE lt_costcenter WHERE sales_off EQ '' AND matl_group EQ '' AND /bic/zmar EQ ''.

          lt_clave_venta = CORRESPONDING #( lt_costcenter ).
          DELETE ADJACENT DUPLICATES FROM lt_clave_venta.
        ENDIF.



*------------------------------------------------------------------------
***VENTAS FORECAST (fuente IBM - Verios 001, 002, etc) - ZCOP_A03

*2) Selecciona la venta FORECAST para calcular el Gasto Forecast = ( Vta Forcast % Part) * Coef

        CLEAR lr_fiscper_vta[].
        LOOP AT o_utiles->gt_period_vta ASSIGNING FIELD-SYMBOL(<fs_period_var>).

          wr_fiscper-low = <fs_period_var>-plan.
          wr_fiscper-option = 'EQ'.
          wr_fiscper-sign   = 'I'.
          APPEND wr_fiscper TO lr_fiscper_vta.

        ENDLOOP.


*2.1) Busca Vta por CECO
        SELECT a~version, a~comp_code, a~co_area, a~cebe AS ceco, fiscper, cant AS canttotal, ventas AS ventastotal
        INTO TABLE @DATA(lt_vta_ceco)
        FROM zpl_vtas_for_01( p_datum = @sy-datum ) AS a
        FOR ALL ENTRIES IN @lt_clave_venta
        WHERE a~version EQ @i_version
        AND   a~co_area   EQ @lt_clave_venta-co_area
        AND   a~cebe      EQ @lt_clave_venta-costcenter.
        IF sy-subrc EQ 0.
          SORT lt_vta_ceco BY version comp_code fiscper ceco.
        ENDIF.

*2.2) Busca por Sucursal, Linea y Marca
        SELECT *
       INTO TABLE @DATA(lt_vta_pres)
       FROM
        zpl_vtas_for_01_sum( i_datum = @sy-datum ) AS a
        FOR ALL ENTRIES IN @lt_clave_venta
      WHERE a~version EQ @i_version
       AND  a~co_area EQ @lt_clave_venta-co_area
       AND  a~fiscper IN @lr_fiscper_vta
       AND   a~sucursal EQ @lt_clave_venta-sales_off
*      AND   a~linea EQ @lt_clave_venta-matl_group
      AND   a~marca    EQ @lt_clave_venta-/bic/zmar
      AND   a~tipo EQ 'venta'.
        IF sy-subrc EQ 0.
          SORT lt_vta_pres BY version comp_code fiscper sucursal
*          linea
          marca.
        ENDIF.
*
**2.3) Busca por Sucursal y Linea:
*        SELECT *
*          INTO TABLE @DATA(lt_vta_pres_lin_suc)
*          FROM
*           zpl_vtas_for_02_sum( i_datum = @sy-datum ) AS a
*           FOR ALL ENTRIES IN @lt_clave_venta
*         WHERE a~version EQ @i_version
*       AND  a~co_area EQ @lt_clave_venta-co_area
*          AND  a~fiscper IN @lr_fiscper_vta
*         AND   a~sucursal EQ @lt_clave_venta-sales_off
*         AND   a~linea EQ @lt_clave_venta-matl_group.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_pres_lin_suc BY version comp_code fiscper sucursal linea .
*        ENDIF.

*10/5/2024: Corta la búsqueda en Sucursal y Linea
*---------------------------------------------------------------
*
*        SELECT *
*          INTO TABLE @DATA(lt_vta_pres_suc)
*          FROM
*           zpl_vtas_for_03_sum( i_datum = @sy-datum ) AS a
*           FOR ALL ENTRIES IN @lt_clave_venta
*         WHERE a~version EQ @i_version
*       AND  a~co_area EQ @lt_clave_venta-co_area
*         AND  a~fiscper IN @lr_fiscper_vta
*         AND   a~sucursal EQ @lt_clave_venta-sales_off.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_pres_suc BY version comp_code fiscper sucursal.
*        ENDIF.
*------------------------------------------------------------------------






*5) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
        SELECT *
       INTO TABLE @DATA(lt_glaccount)
       FROM /bic/azpl_a292 AS a
       FOR ALL ENTRIES IN @lt_claves
       WHERE a~username  EQ @lt_claves-username
       AND   a~comp_code EQ @lt_claves-comp_code
       AND   a~/bic/zpl_dum01 EQ '1'.
        IF sy-subrc EQ 0.
          SORT lt_glaccount BY comp_code username gl_account.
        ENDIF.


**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************
        LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


******* Agrupadores Sucursal, LInea Marca
          READ TABLE lt_costcenter ASSIGNING FIELD-SYMBOL(<fs_cost>) WITH KEY costcenter = <fs_clave>-costcenter.
          CHECK sy-subrc EQ 0.

          CLEAR: l_coef,  wa_result.

*     Inicializa wa_result:
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          MOVE-CORRESPONDING <fs_cost> TO wa_result.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-/bic/ztabla = me->c_forecast.
          wa_result-unit    = me->c_unit.
          wa_result-currency = me->c_pyg.
          wa_result-version = i_version.



*     Agrega los 12 periodos para cada Clave:
          LOOP AT me->gt_period_vta ASSIGNING FIELD-SYMBOL(<fs_perio>).

            CLEAR:  wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp, wa_result-/bic/zpl_ventq,
                    wa_result-/bic/zpl_vtaqp.


*----------------------------------------*
* VENTA FORECAST
*----------------------------------------*
            READ TABLE lt_vta_ceco ASSIGNING FIELD-SYMBOL(<fs_vta_ceco>) WITH KEY
                                                                           version  = i_version
                                                                           comp_code = <fs_clave>-comp_code
                                                                           fiscper   = <fs_perio>-plan
                                                                           ceco      = <fs_clave>-costcenter BINARY SEARCH.
            IF sy-subrc EQ 0.
              wa_result-/bic/zpl_ventp =  abs( <fs_vta_ceco>-ventastotal ).
              wa_result-/bic/zpl_vtaqp =  abs( <fs_vta_ceco>-canttotal ).


            ELSE.


              READ TABLE lt_vta_pres ASSIGNING FIELD-SYMBOL(<fs_presup>) WITH KEY
                                                                             version  = i_version
                                                                             comp_code = <fs_clave>-comp_code
                                                                             fiscper   = <fs_perio>-plan
                                                                             sucursal  = <fs_cost>-sales_off
*                                                                             linea     = <fs_cost>-matl_group
                                                                             marca     = <fs_cost>-/bic/zmar BINARY SEARCH.
              IF sy-subrc EQ 0.

                wa_result-/bic/zpl_ventp =  abs( <fs_presup>-ventastotal ).
                wa_result-/bic/zpl_vtaqp =  abs( <fs_presup>-canttotal ).
              ELSE.

*          Busca las ventas agrupadas por Linea y Sucursal
*                READ TABLE lt_vta_pres_lin_suc ASSIGNING FIELD-SYMBOL(<fs_suc_lin_pre>) WITH KEY
*                                                                             version  = i_version
*                                                                             comp_code = <fs_clave>-comp_code
*                                                                             fiscper   = <fs_perio>-plan
*                                                                             sucursal  = <fs_cost>-sales_off
*                                                                             linea     = <fs_cost>-matl_group
*                                                                             BINARY SEARCH.
*                IF sy-subrc EQ 0.
*                  wa_result-/bic/zpl_ventp = abs( <fs_suc_lin_pre>-ventastotal ).
*                  wa_result-/bic/zpl_vtaqp = <fs_suc_lin_pre>-canttotal.
*                ELSE.
*            Busca las ventas agrupadas por sucursal
*                READ TABLE lt_vta_pres_suc ASSIGNING FIELD-SYMBOL(<fs_suc_pre>)  WITH KEY
*                                                                                 version  = o_utiles->c_forecast
*                                                                                comp_code = <fs_clave>-comp_code
*                                                                             fiscper   = <fs_perio>-plan
*                                                                             sucursal  = <fs_cost>-sales_off
*                                                                             BINARY SEARCH.
*                IF sy-subrc EQ 0.
*                  READ TABLE lt_glaccount WITH KEY    comp_code   = <fs_clave>-comp_code
*                                                      username    = <fs_clave>-username
*                                                      gl_account  = <fs_clave>-gl_account
*                                                      BINARY SEARCH TRANSPORTING NO FIELDS.
*                  IF sy-subrc NE 0. "La cuenta no es Excepción, entonces llega a nivel Oficina de Ventas
*                    wa_result-/bic/zpl_ventp = abs( <fs_suc_pre>-ventastotal ).
*                    wa_result-/bic/zpl_vtaqp = abs( <fs_suc_pre>-canttotal ).
              ENDIF."CECO
            ENDIF."Sucursal y Marca
*            ENDIF."Linea, Sucursal
*            ENDIF."Sucursal, Linea Marca

            wa_result-/bic/zpl_ventp = abs( wa_result-/bic/zpl_ventp  ).
            wa_result-/bic/zpl_vtaqp = abs( wa_result-/bic/zpl_vtaqp ).


*------------------------------------
            wa_result-fiscper  = <fs_perio>-plan.
            wa_result-fiscyear = wa_result-fiscper(4).

            APPEND wa_result TO lt_aux.

          ENDLOOP." Periodos
        ENDLOOP." Claves Source Package

        e_result = lt_aux[].




      CATCH  cx_root INTO DATA(l_excepcion).
    ENDTRY.




  ENDMETHOD."Forecast Sociedad 1000


  METHOD get_vta_fore_2000.



    DATA:
      wa_result TYPE ty_wa_result_vta,
      lt_aux    TYPE ty_lt_result_vta.

    DATA: o_utiles       TYPE REF TO zpl_utiles,
          l_coef         TYPE p DECIMALS 4,
          lr_fiscper_re  TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_pre TYPE RANGE OF /bi0/oifiscper,
          lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper     LIKE LINE OF lr_fiscper_re,
*          lt_glaccount   TYPE RANGE OF /bi0/oigl_account,
          lt_claves      TYPE ty_lt_claves, "ordenada
          lt_clave_venta TYPE ty_lt_clave_venta_2000.


    CHECK i_claves[] IS NOT INITIAL.

    TRY.

        o_utiles =  NEW zpl_utiles( ).

        lt_claves[] = i_claves[].
        DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
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



*------------------------------------------------------------------------
*1) Obtengo para cada CEBE: Linea, Of de Venta y Marca

*    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
*    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

        SELECT
        b~co_area,
        b~costcenter,
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

          lt_clave_venta = CORRESPONDING #( lt_costcenter ).
          DELETE ADJACENT DUPLICATES FROM lt_clave_venta.
        ENDIF.



*----------------------------------------------
***VENTAS FORECAST (fuente IBM - I14 por ejemplo) - ZCOP_A03
*2) Selecciona la venta FORECAST para calcular el Gasto Presupuestado = Vta Pres * Coef

        CLEAR lr_fiscper_vta[].
        LOOP AT o_utiles->gt_period_vta ASSIGNING <fs_period_var>.

          wr_fiscper-low = <fs_period_var>-plan.
          wr_fiscper-option = 'EQ'.
          wr_fiscper-sign   = 'I'.
          APPEND wr_fiscper TO lr_fiscper_vta.

        ENDLOOP.

        IF lr_fiscper_pre[] IS NOT INITIAL.
          APPEND LINES OF lr_fiscper_pre TO lr_fiscper_vta.
        ENDIF.



*4.1) Busca Vta por CECO
        SELECT a~version, a~comp_code, a~co_area, a~cebe AS ceco, fiscper, cant AS canttotal, ventas AS ventastotal
        INTO TABLE @DATA(lt_vta_ceco)
        FROM zpl_vtas_for_01( p_datum = @sy-datum ) AS a
        FOR ALL ENTRIES IN @lt_clave_venta
        WHERE a~version EQ @i_version
        AND   a~co_area   EQ @lt_clave_venta-co_area
        AND   a~cebe      EQ @lt_clave_venta-costcenter.
        IF sy-subrc EQ 0.
          SORT lt_vta_ceco BY version comp_code fiscper ceco.
        ENDIF.

*4.2)Clave completa: Sucursal, Canal, Linea, Marca
        SELECT *
       INTO TABLE @DATA(lt_vta_pres_s1)
       FROM
        zpl_vtas_for_s2_01_sum( i_datum = @sy-datum ) AS a
        FOR ALL ENTRIES IN @lt_clave_venta
      WHERE a~version EQ @i_version
       AND a~co_area EQ @lt_clave_venta-co_area
       AND  a~fiscper IN @lr_fiscper_vta
      AND   a~sucursal EQ @lt_clave_venta-sales_off
      AND   a~canal    EQ @lt_clave_venta-distr_chan
      AND   a~linea EQ @lt_clave_venta-matl_group
      AND   a~marca    EQ @lt_clave_venta-/bic/zmar
      AND   a~tipo EQ 'venta'.
        IF sy-subrc EQ 0.
          SORT lt_vta_pres_s1 BY version comp_code fiscper sucursal canal linea marca.
        ENDIF.


*4,3) Sucursal, Canal, Linea:S2
        SELECT *
          INTO TABLE @DATA(lt_vta_pres_s2)
          FROM
           zpl_vtas_for_s2_02_sum( i_datum = @sy-datum ) AS a
           FOR ALL ENTRIES IN @lt_clave_venta
         WHERE a~version EQ @i_version
       AND a~co_area EQ @lt_clave_venta-co_area
         AND   a~fiscper IN @lr_fiscper_vta
         AND   a~sucursal EQ @lt_clave_venta-sales_off
         AND   a~canal    EQ @lt_clave_venta-distr_chan
         AND   a~linea EQ @lt_clave_venta-matl_group.
        IF sy-subrc EQ 0.
          SORT lt_vta_pres_s2 BY version comp_code fiscper sucursal canal linea.
        ENDIF.
*
*
**4.3) Sucursal, Canal: S3
*        SELECT *
*          INTO TABLE @DATA(lt_vta_pres_s3)
*          FROM
*           zpl_vtas_for_s2_03_sum( i_datum = @sy-datum ) AS a
*           FOR ALL ENTRIES IN @lt_clave_venta
*        WHERE a~version EQ @i_version
*       AND a~co_area EQ @lt_clave_venta-co_area
*         AND   a~fiscper IN @lr_fiscper_vta
*         AND   a~sucursal EQ @lt_clave_venta-sales_off
*         AND   a~canal    EQ @lt_clave_venta-distr_chan.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_pres_s3 BY version comp_code fiscper sucursal canal.
*        ENDIF.
*
**4.4) Sucursal: S4
*        SELECT *
*          INTO TABLE @DATA(lt_vta_pres_s4)
*          FROM
*           zpl_vtas_for_s2_04_sum( i_datum = @sy-datum ) AS a
*           FOR ALL ENTRIES IN @lt_clave_venta
*        WHERE a~version EQ @i_version
*       AND a~co_area EQ @lt_clave_venta-co_area
*         AND  a~fiscper IN @lr_fiscper_vta
*         AND   a~sucursal EQ @lt_clave_venta-sales_off.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_pres_s4 BY version comp_code fiscper sucursal.
*        ENDIF.
*
*





*5) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
*        READ TABLE lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>)  INDEX 1.

*        SELECT 'I' AS sign, 'EQ' AS option, a~gl_account
        SELECT *
       INTO TABLE @DATA(lt_glaccount)
       FROM /bic/azpl_a292 AS a
       FOR ALL ENTRIES IN @lt_claves
       WHERE a~username  EQ @lt_claves-username
       AND   a~comp_code EQ @lt_claves-comp_code
       AND   a~/bic/zpl_dum01 EQ '1'.
        IF sy-subrc EQ 0.
          SORT lt_glaccount BY comp_code username gl_account.
        ENDIF.




**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************
        LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


******* Agrupadores Sucursal, LInea Marca
          READ TABLE lt_costcenter ASSIGNING FIELD-SYMBOL(<fs_cost>) WITH KEY costcenter = <fs_clave>-costcenter.
          CHECK sy-subrc EQ 0.

          CLEAR: l_coef,  wa_result.

*     Inicializa wa_result:
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          MOVE-CORRESPONDING <fs_cost> TO wa_result.
          wa_result-costcenter = <fs_cost>-costcenter.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-/bic/ztabla = me->c_plan.
          wa_result-version = me->c_version.
          wa_result-unit    = 'UN'.
          wa_result-currency = 'PYG'.




*     Agrega los 12 periodos para cada Clave:
          LOOP AT o_utiles->gt_period_var ASSIGNING FIELD-SYMBOL(<fs_perio>).

            CLEAR: wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp, wa_result-/bic/zpl_ventq, wa_result-/bic/zpl_vtaqp.


*----------------------------------------*
* VENTA FORECAST (I14 por ejemplo)
*----------------------------------------*
*           1) Busca por CECO
            READ TABLE lt_vta_ceco ASSIGNING FIELD-SYMBOL(<fs_vta_ceco>) WITH KEY
                                                                           version  = i_version
                                                                           comp_code = <fs_clave>-comp_code
                                                                           fiscper   = <fs_perio>-plan
                                                                           ceco      = <fs_clave>-costcenter BINARY SEARCH.
            IF sy-subrc EQ 0.
              wa_result-/bic/zpl_ventp =  abs( <fs_vta_ceco>-ventastotal ).
              wa_result-/bic/zpl_vtaqp =  abs( <fs_vta_ceco>-canttotal ).


            ELSE.


*           2) Busca por clave completa:
              READ TABLE lt_vta_pres_s1 ASSIGNING FIELD-SYMBOL(<fs_venta_p1>)  WITH KEY version   = i_version
                                                                                    comp_code = <fs_clave>-comp_code
                                                                                    fiscper   = <fs_perio>-plan
                                                                                    sucursal  = <fs_cost>-sales_off
                                                                                    canal     = <fs_cost>-distr_chan
                                                                                    linea     = <fs_cost>-matl_group
                                                                                    marca     = <fs_cost>-/bic/zmar
                                                                                    BINARY SEARCH.

              IF sy-subrc EQ 0.
                wa_result-/bic/zpl_ventp =  abs( <fs_venta_p1>-ventastotal ).
                wa_result-/bic/zpl_vtaqp = abs( <fs_venta_p1>-canttotal ).

              ELSE.
*          3) Busca las ventas agrupadas por  Sucursal, Canal, Linea
                READ TABLE lt_vta_pres_s2 ASSIGNING FIELD-SYMBOL(<fs_venta_p2>) WITH KEY version   = i_version
                                                                              comp_code = <fs_clave>-comp_code
                                                                              fiscper   = <fs_perio>-plan
                                                                             sucursal  = <fs_cost>-sales_off
                                                                             canal     = <fs_cost>-distr_chan
                                                                             linea     = <fs_cost>-matl_group
                                                                             BINARY SEARCH.
                IF sy-subrc EQ 0.
                  wa_result-/bic/zpl_ventp = abs( <fs_venta_p2>-ventastotal  ).
                  wa_result-/bic/zpl_vtaqp = abs( <fs_venta_p2>-canttotal ).
                ELSE.
**            Busca las ventas agrupadas por sucursal, canal
*                READ TABLE lt_vta_pres_s3 ASSIGNING FIELD-SYMBOL(<fs_venta_p3>) WITH KEY version   = i_version
*                                                                             comp_code = <fs_clave>-comp_code
*                                                                             fiscper   = <fs_perio>-plan
*                                                                             sucursal  = <fs_cost>-sales_off
*                                                                             canal     = <fs_cost>-distr_chan
*                                                                             BINARY SEARCH.
*                IF sy-subrc EQ 0.
*                  wa_result-/bic/zpl_ventp = abs( <fs_venta_p3>-ventastotal  ).
*                  wa_result-/bic/zpl_vtaqp = abs( <fs_venta_p3>-canttotal ).
*
*                ELSE.
**            Busca las ventas agrupadas por sucursal
*                  READ TABLE lt_vta_pres_s4 ASSIGNING FIELD-SYMBOL(<fs_venta_p4>)  WITH KEY version   = i_version
*                                                                               comp_code = <fs_clave>-comp_code
*                                                                               fiscper   = <fs_perio>-plan
*                                                                               sucursal  = <fs_cost>-sales_off
*                                                                               BINARY SEARCH.
*                  IF sy-subrc EQ 0.
*                    READ TABLE lt_glaccount WITH KEY comp_code   = <fs_clave>-comp_code
*                                                     username    = <fs_clave>-username
*                                                     gl_account  = <fs_clave>-gl_account
*                                                     BINARY SEARCH TRANSPORTING NO FIELDS.
*                    IF sy-subrc NE 0. "La cuenta no es Excepción, entonces llega a nivel Oficina de Ventas
*                      wa_result-/bic/zpl_ventp = abs(  <fs_venta_p4>-ventastotal  ).
*                      wa_result-/bic/zpl_vtaqp = abs( <fs_venta_p4>-canttotal ).
*                    ENDIF.
*                  ENDIF."Sucursal
                ENDIF."1 - CECO
              ENDIF."2 - Sucursal, Canal, LInea, marca
            ENDIF."3 - Sucursal, Linea



*--------------------------------------------------------

            CALL METHOD o_utiles->get_plan_period
              EXPORTING
                i_tipo   = 'VAR'
                i_period = <fs_perio>-actual "Periodo de input/precarga
              IMPORTING
                o_plan   = wa_result-fiscper. "Periodo Plan


            wa_result-fiscyear = wa_result-fiscper(4).

            APPEND wa_result TO lt_aux.

          ENDLOOP." Periodos
        ENDLOOP." Claves Source Package

        e_result = lt_aux[].

      CATCH  cx_root INTO DATA(l_excepcion).

    ENDTRY.



  ENDMETHOD.


  METHOD get_vta_presup.



*********************************************************************************************
    DATA: o_utiles TYPE REF TO zpl_utiles.
    DATA: l_coef TYPE p DECIMALS 4.
    DATA:
*          lr_fiscper_re  TYPE RANGE OF /bi0/oifiscper,
*          lr_fiscper_pre TYPE RANGE OF /bi0/oifiscper,
      lr_fiscper_vta TYPE RANGE OF /bi0/oifiscper,
      wr_fiscper     LIKE LINE OF lr_fiscper_vta.

    DATA: wa_result      TYPE ty_wa_result_vta,
          lt_claves      TYPE ty_lt_claves, "ordenada
          lt_clave_venta TYPE ty_lt_clave_venta,
          lt_aux         TYPE ty_lt_result_vta.



    TRY.
        o_utiles =  NEW zpl_utiles( ).

        lt_claves[] = i_claves[].
        DELETE ADJACENT DUPLICATES FROM lt_claves COMPARING comp_code co_area gl_account costcenter coorder username.
*-----------------------------------------------------------------------------
*1) Obtengo para cada CEBE: Linea, Of de Venta y Marca

*    DATA(lt_aux_sort) = CORRESPONDING ty_lt_aux( result_package ).
*    DELETE ADJACENT DUPLICATES FROM lt_aux_sort COMPARING comp_code gl_account profit_ctr coorder.

        SELECT
        b~co_area,
        b~costcenter,
        b~sales_off,
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
          DELETE lt_costcenter WHERE sales_off EQ '' AND matl_group EQ '' AND /bic/zmar EQ ''.

          lt_clave_venta = CORRESPONDING #( lt_costcenter ).
          DELETE ADJACENT DUPLICATES FROM lt_clave_venta.
        ENDIF.


*------------------------------------------------------------------------
***VENTAS PRESUPUESTADAS (fuente COPA - 24V0 por ejemplo) - CDS sobre ZPL_A35
*4) Selecciona la venta PRESUPUESTADA para calcular el Gasto Presupuestado = Vta Pres * Coef
*   Además puede incluir Venta Presupuestada del año en curso en vez de Reales
        CLEAR lr_fiscper_vta[].
        LOOP AT o_utiles->gt_period_var ASSIGNING FIELD-SYMBOL(<fs_period_vta>).

          wr_fiscper-low = <fs_period_vta>-plan.
          wr_fiscper-option = 'EQ'.
          wr_fiscper-sign   = 'I'.
          APPEND wr_fiscper TO lr_fiscper_vta.

        ENDLOOP.

*        IF NOT lr_fiscper_pre[] IS INITIAL.
*          APPEND LINES OF lr_fiscper_pre TO lr_fiscper_vta.
*        ENDIF.


        IF lt_clave_venta[] IS NOT INITIAL.
*1) Busca la Venta Presup. utilizando el CECO.
          SELECT
              a~comp_code,
              a~co_area,
              a~fiscper,
              a~cebe,
              a~ventas AS ventastotal,
              a~cant   AS canttotal
           INTO TABLE @DATA(lt_vta_ceco)
           FROM
            zpl_cebes_pres( p_datum = @sy-datum ) AS a
            FOR ALL ENTRIES IN @lt_clave_venta
          WHERE a~co_area EQ @lt_clave_venta-co_area
           AND  a~fiscper IN @lr_fiscper_vta
           AND  a~cebe EQ @lt_clave_venta-costcenter
           AND  a~tipo EQ 'venta'
           AND  a~tabla EQ @i_tabla. "me->c_plan.

          IF sy-subrc EQ 0.
            SORT lt_vta_ceco BY comp_code fiscper cebe.
          ENDIF.



*2) Busca la venta por Sucursal, Linea y Marca

          SELECT
            a~comp_code,
            a~co_area,
            a~fiscper,
            a~linea,
            a~sucursal,
            a~marca,
            a~gastostotal,
            a~ventastotal,
            a~canttotal
         INTO TABLE @DATA(lt_vta_pres)
         FROM
          zpl_cebes_pres_u( i_datum = @sy-datum ) AS a
          FOR ALL ENTRIES IN @lt_clave_venta
        WHERE a~co_area EQ @lt_clave_venta-co_area
         AND  a~fiscper IN @lr_fiscper_vta
         AND   a~sucursal EQ @lt_clave_venta-sales_off
         AND   a~linea EQ @lt_clave_venta-matl_group
         AND   a~marca    EQ @lt_clave_venta-/bic/zmar
         AND   a~tipo EQ 'venta'
         AND  a~tabla EQ @i_tabla. " me->c_plan.
*         AND   a~tabla EQ @me->c_plan.
          IF sy-subrc EQ 0.
            SORT lt_vta_pres BY comp_code fiscper sucursal linea marca.
          ENDIF.

*3) Busca Venta Presup por Sucursal y Linea
          SELECT *
            INTO TABLE @DATA(lt_vta_pres_lin_suc)
            FROM
             zpl_vtas_pres_suc_linea( i_datum = @sy-datum ) AS a
             FOR ALL ENTRIES IN @lt_clave_venta
           WHERE a~co_area EQ @lt_clave_venta-co_area
            AND  a~fiscper IN @lr_fiscper_vta
            AND   a~sucursal EQ @lt_clave_venta-sales_off
            AND   a~linea EQ @lt_clave_venta-matl_group
            AND  a~tabla EQ @i_tabla. "me->c_plan.
*            AND   a~tabla EQ @me->c_plan.
          IF sy-subrc EQ 0.
            SORT lt_vta_pres_lin_suc BY comp_code fiscper sucursal linea .
          ENDIF.



*4) Busca Venta Presup por Sucursal
*        SELECT *
*          INTO TABLE @DATA(lt_vta_pres_suc)
*          FROM
*           zpl_vtas_pres_suc_( i_datum = @sy-datum ) AS a
*           FOR ALL ENTRIES IN @lt_clave_venta
*         WHERE  a~co_area EQ @lt_clave_venta-co_area
*         AND  a~fiscper IN @lr_fiscper_vta
*         AND   a~sucursal EQ @lt_clave_venta-sales_off.
*        IF sy-subrc EQ 0.
*          SORT lt_vta_pres_suc BY comp_code fiscper sucursal.
*        ENDIF.
*------------------------------------------------------------------------

        ENDIF."lt_clave_ventas[] not initial




*5) Obtengo CUENTAS que no deben calcular la Venta Agrupada por Sucursal.
        SELECT *
       INTO TABLE @DATA(lt_glaccount)
       FROM /bic/azpl_a292 AS a
       FOR ALL ENTRIES IN @lt_claves
       WHERE a~username  EQ @lt_claves-username
       AND   a~comp_code EQ @lt_claves-comp_code
       AND   a~/bic/zpl_dum01 EQ '1'.
        IF sy-subrc EQ 0.
          SORT lt_glaccount BY comp_code username gl_account.
        ENDIF.


**********************************************************************
* INICIO PROCESO: CLAVES son FISCPER, SOCIEDAD, CUENTA, CEBE Y ORDEN
**********************************************************************
        LOOP AT  lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).


******* Agrupadores Sucursal, LInea Marca
          READ TABLE lt_costcenter ASSIGNING FIELD-SYMBOL(<fs_cost>) WITH KEY costcenter = <fs_clave>-costcenter.
          CHECK sy-subrc EQ 0.

          CLEAR: l_coef,  wa_result.

*     Inicializa wa_result:
          MOVE-CORRESPONDING <fs_clave> TO wa_result.
          MOVE-CORRESPONDING <fs_cost> TO wa_result.
          wa_result-chrt_accts = 'SQS'.
          wa_result-fiscvarnt  = 'K4'.
          wa_result-/bic/ztabla = i_tabla.
          wa_result-unit    = me->c_unit.
          wa_result-currency = me->c_pyg.
          wa_result-version = me->c_version.


*     Agrega los 12 periodos para cada Clave:
          LOOP AT me->gt_period_var ASSIGNING FIELD-SYMBOL(<fs_perio>).

            CLEAR:  wa_result-/bic/zpl_venta, wa_result-/bic/zpl_ventp, wa_result-/bic/zpl_ventq,
                    wa_result-/bic/zpl_vtaqp.

**********************************************************************----------------------------------------*
* VENTA RESUPUESTADA (25V0)
*----------------------------------------*
*1) Busca solo por CECO:


            READ TABLE lt_vta_ceco ASSIGNING FIELD-SYMBOL(<fs_vtaceco>) WITH KEY      comp_code = <fs_clave>-comp_code
                                                                                      fiscper = <fs_perio>-plan
                                                                                      cebe   = <fs_clave>-costcenter
                                                                                      BINARY SEARCH.
            IF sy-subrc EQ 0.
              wa_result-/bic/zpl_ventp =  abs( <fs_vtaceco>-ventastotal ).
              wa_result-/bic/zpl_vtaqp =  abs( <fs_vtaceco>-canttotal ).
            ELSE.

*           2) Busca por Sucursal, linea y marca:
              READ TABLE lt_vta_pres ASSIGNING FIELD-SYMBOL(<fs_presup>) WITH KEY   comp_code = <fs_clave>-comp_code
                                                                               fiscper   = <fs_perio>-plan
                                                                               sucursal  = <fs_cost>-sales_off
                                                                                linea     = <fs_cost>-matl_group
                                                                               marca     = <fs_cost>-/bic/zmar
                                                                               BINARY SEARCH.
              IF sy-subrc EQ 0.

                wa_result-/bic/zpl_ventp =  abs( <fs_presup>-ventastotal  ).
                wa_result-/bic/zpl_vtaqp =  abs( <fs_presup>-canttotal ).
              ELSE.

*          3) Busca las ventas agrupadas por Linea y Sucursal
                READ TABLE lt_vta_pres_lin_suc ASSIGNING FIELD-SYMBOL(<fs_suc_lin_pre>) WITH KEY   comp_code = <fs_clave>-comp_code
                                                                                 fiscper   = <fs_perio>-plan
                                                                                 sucursal  = <fs_cost>-sales_off
                                                                                linea     = <fs_cost>-matl_group
                                                                              BINARY SEARCH.
                IF sy-subrc EQ 0.
                  wa_result-/bic/zpl_ventp = abs( <fs_suc_lin_pre>-ventastotal  ).
                  wa_result-/bic/zpl_vtaqp = <fs_suc_lin_pre>-canttotal.
                ELSE.
*           4)  Busca las ventas agrupadas por sucursal
*                  READ TABLE lt_vta_pres_suc ASSIGNING FIELD-SYMBOL(<fs_suc_pre>)  WITH KEY   comp_code = <fs_clave>-comp_code
*                                                                                   fiscper   = <fs_perio>-actual
*                                                                                   sucursal  = <fs_cost>-sales_off
*                                                                                  BINARY SEARCH.
*                  IF sy-subrc EQ 0.
*                    READ TABLE lt_glaccount WITH KEY comp_code   = <fs_clave>-comp_code
*                                                        username    = <fs_clave>-username
*                                                        gl_account  = <fs_clave>-gl_account
*                                                        BINARY SEARCH TRANSPORTING NO FIELDS.
*                    IF sy-subrc NE 0. "La cuenta no es Excepción, entonces llega a nivel Oficina de Ventas
*                      wa_result-/bic/zpl_ventp = abs( <fs_suc_pre>-ventastotal  ).
*                      wa_result-/bic/zpl_vtaqp = abs( <fs_suc_pre>-canttotal ).
*                    ENDIF.
*                  ENDIF."Sucursal
                ENDIF."Linea, Sucursal
              ENDIF."Sucursal, Linea Marca
            ENDIF."CECO unicamente
            wa_result-/bic/zpl_ventp = abs( wa_result-/bic/zpl_ventp  ).
            wa_result-/bic/zpl_vtaqp = abs( wa_result-/bic/zpl_vtaqp ).


*------------------------------------

            CALL METHOD o_utiles->get_plan_period
              EXPORTING
                i_tipo   = 'VAR'
                i_period = <fs_perio>-actual "Periodo de input/precarga
              IMPORTING
                o_plan   = wa_result-fiscper. "Periodo Plan


            wa_result-fiscyear = wa_result-fiscper(4).

            APPEND wa_result TO lt_aux.

          ENDLOOP." Periodos
        ENDLOOP." Claves Source Package

        e_result = lt_aux[].




      CATCH  cx_root INTO DATA(l_excepcion).
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
