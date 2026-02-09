CLASS zpl_abm_f12 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rsplfa_srvtype_imp_exec .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpl_abm_f12 IMPLEMENTATION.


  METHOD if_rsplfa_srvtype_imp_exec~execute.
    DATA l_crear        TYPE i VALUE 1."Por defecto se crea el nuevo registro
    DATA lo_utiles      TYPE REF TO zpl_utiles.
    DATA wa_clave       TYPE zpl_utiles=>ty_wa_clave.
    DATA l_tipo         TYPE char4.
    DATA l_tipo_a       TYPE char4.
    DATA lr_data        TYPE REF TO data.
    DATA lr_tab         TYPE REF TO data.
    DATA l_r_param_elem TYPE REF TO if_rsplfa_param_elem.
    DATA l_msg          TYPE if_rspls_cr_types=>tn_s_mesg.
    DATA l_fiscyear     TYPE /bi0/oifiscyear.
    DATA l_fiscper      TYPE /bi0/oifiscper.
    DATA l_periodo      TYPE numc3.
    DATA l_infoprov     TYPE rsinfoprov.
    DATA l_version      TYPE /bi0/oiversion.
    DATA l_status        TYPE int8.
    DATA: BEGIN OF wa_field,
            in  TYPE char14,
            out TYPE char14,
          END OF wa_field.

    FIELD-SYMBOLS <s>           TYPE any.
    FIELD-SYMBOLS <f>           TYPE any.
    FIELD-SYMBOLS <o>           TYPE any.
    FIELD-SYMBOLS <fs_new_data> TYPE HASHED TABLE.
    FIELD-SYMBOLS <fs_fields>   TYPE STANDARD TABLE.


*Esta rutina resuelve tres casos:
*1) La combinación existia antes y se debe reasignar al nuevo usuario
*2) Ya estaba reasignada al usuario y no debe realizarse modificaciones en los ratios
*3) La combinación debe crearse desde cero y asignarse al nuevo usuario.




    " Crea una tabla temporaria para guardar los nuevos registros:
    CREATE DATA lr_tab LIKE c_th_data.
    ASSIGN lr_tab->* TO <fs_new_data>.

    TRY.
        lo_utiles = NEW zpl_utiles( ).

        l_r_param_elem = i_r_param_set->get_param_elem( '0USERNAME' ).
        l_r_param_elem->get_value( IMPORTING e_value = wa_clave-username  ).

        l_r_param_elem = i_r_param_set->get_param_elem( '0COMP_CODE' ).
        l_r_param_elem->get_value( IMPORTING e_value = wa_clave-comp_code  ).

        l_r_param_elem = i_r_param_set->get_param_elem( '0CO_AREA' ).
        l_r_param_elem->get_value( IMPORTING e_value = wa_clave-co_area  ).

        l_r_param_elem = i_r_param_set->get_param_elem( '0GL_ACCOUNT' ).
        l_r_param_elem->get_value( IMPORTING e_value = wa_clave-gl_account  ).

        l_r_param_elem = i_r_param_set->get_param_elem( '0COSTCENTER' ).
        l_r_param_elem->get_value( IMPORTING e_value = wa_clave-costcenter  ).

        l_r_param_elem = i_r_param_set->get_param_elem( '0COORDER' ).
        l_r_param_elem->get_value( IMPORTING e_value = wa_clave-coorder  ).

        l_r_param_elem = i_r_param_set->get_param_elem( '0FISCYEAR' ).
        l_r_param_elem->get_value( IMPORTING e_value = l_fiscyear  ).

*        l_r_param_elem = i_r_param_set->get_param_elem( '0INFOPROV' ).
*        l_r_param_elem->get_value( IMPORTING e_value = l_infoprov  ).

        l_r_param_elem = i_r_param_set->get_param_elem( '0VERSION' ).
        l_r_param_elem->get_value( IMPORTING e_value = l_version  ).

        FREE l_r_param_elem.
        DATA: l_ibm     TYPE char4, l_tecnico TYPE char6.

        " Validate the key using zpl_utiles->check_clave
        lo_utiles->check_clave_abm( EXPORTING i_soc    = wa_clave-comp_code
                                          i_soco   = wa_clave-co_area
                                          i_cuenta = wa_clave-gl_account
                                          i_cebe   = wa_clave-costcenter
                                          i_orden  = wa_clave-coorder
                                          i_user   = wa_clave-username
                                IMPORTING o_msg    = l_msg
                                          o_ibm    = l_ibm ).

        " Check the validation result (assuming o_msg indicates success or failure)
        IF l_msg-msgty = 'E'.
          " Handle validation failure by adding a message to i_r_msg
          DATA(l_error_details) =  wa_clave-gl_account && '-' &&
                            wa_clave-costcenter && '-' &&
                            wa_clave-coorder.

          CASE l_msg-msgid.

            WHEN 'CEBE'.


            WHEN 'CUENTA'.

            WHEN 'ORDEN'.

            WHEN OTHERS.

          ENDCASE.


          " TODO: variable is assigned but never used; add pragma ##NEEDED (ABAP cleaner)
          MESSAGE e005(zpl) WITH l_msg-msgid l_ibm wa_clave-username l_error_details INTO DATA(l_dummy)."
          i_r_msg->add_msg( ).
        ELSE.


          DATA: lv_zmar   TYPE /bic/oizmar, lv_status TYPE /bic/oizdir.

          SELECT SINGLE /bic/zmar, /bic/zdir INTO (  @lv_zmar, @lv_status )
            FROM /bi0/mcoorder
            WHERE coorder = @wa_clave-coorder.

          IF lv_status EQ 'I'.

            l_error_details = wa_clave-username && '-' &&
                              wa_clave-gl_account && '-' &&
                              wa_clave-costcenter && '-' &&
                              wa_clave-coorder.
            MESSAGE e008(zpl) WITH l_error_details INTO l_dummy."
            i_r_msg->add_msg( ).

          ENDIF.


          IF lv_zmar IS INITIAL.
            l_error_details = wa_clave-username && '-' &&
                            wa_clave-gl_account && '-' &&
                            wa_clave-costcenter && '-' &&
                            wa_clave-coorder.
            MESSAGE e007(zpl) WITH l_error_details INTO l_dummy."
            i_r_msg->add_msg( ).

          ELSE.

            IF lv_zmar IN lo_utiles->gr_zvar.
              l_infoprov = 'ZPL_A041'.
              l_tipo = 'VAR'.
            ELSEIF lv_zmar IN lo_utiles->gr_zpxq.
              l_infoprov = 'ZPL_A011'.
              l_tipo = 'PXQ'.
            ELSEIF lv_zmar IN lo_utiles->gr_zvarq.
              l_infoprov = 'ZPL_A071'.
              l_tipo = 'VARQ'.
            ELSE.
              l_error_details = wa_clave-username && '-' &&
                wa_clave-gl_account && '-' &&
                wa_clave-costcenter && '-' &&
                wa_clave-coorder.
              MESSAGE e006(zpl) WITH l_error_details lv_zmar INTO l_dummy."
              i_r_msg->add_msg( ).

            ENDIF.
          ENDIF.
*agregar compañia, objve y fecha
          SELECT SINGLE a~/bic/zco_stat INTO @DATA(l_status_ceco)
          FROM /bi0/mcostcenter AS a
          WHERE a~costcenter =  @wa_clave-costcenter
          AND a~comp_code    = @wa_clave-comp_code
          AND a~objvers    = 'A'
          AND a~dateto     = '99991231'.
          IF sy-subrc EQ 0 AND l_status_ceco IS NOT INITIAL.

            l_error_details = wa_clave-username && '-' &&
               wa_clave-gl_account && '-' &&
               wa_clave-costcenter && '-' &&
               wa_clave-coorder.

            MESSAGE e011(zpl) WITH wa_clave-costcenter l_error_details  INTO l_dummy."
            i_r_msg->add_msg( ).

          ENDIF.




          " Assign the appropriate field symbol based on the value of l_infoprov
          CASE l_infoprov.
            WHEN 'ZPL_A031'.
              ASSIGN lo_utiles->gt_fields_abm TO <fs_fields>.
            WHEN 'ZPL_A011'.
              ASSIGN lo_utiles->gt_fields_pxq TO <fs_fields>.
            WHEN 'ZPL_A041'.
              ASSIGN lo_utiles->gt_fields_var TO <fs_fields>.
            WHEN 'ZPL_A071'.
              ASSIGN lo_utiles->gt_fields_varq TO <fs_fields>.
            WHEN OTHERS.
              RAISE EXCEPTION TYPE cx_sy_no_handler
                EXPORTING
                  textid = 'Invalid InfoProvider'.
          ENDCASE.


          " Casos que pueden ocurrir:
          "1. La combinacion no existe -> c_th_data[] = vacio
          "2. La combinación existe y debe ser asignada al nuevo usuario -> c_th_data[] solo combinacion del usuario viejo
          "3. Las combinaciones del usuario viejo y el nuevo existen -> c_th_data[] con ambas combinaciones .

          "==================== REEMPLAZO DESDE AQUÍ ====================
          IF c_th_data[] IS NOT INITIAL.

            DATA lv_combo_found       TYPE abap_bool VALUE abap_false.
            DATA lv_new_exists        TYPE abap_bool VALUE abap_false.
            DATA lv_old_active_exists TYPE abap_bool VALUE abap_false.

            " Flags por período para evitar duplicar clones si hay múltiples líneas del mismo período
            DATA lv_cloned_once TYPE abap_bool VALUE abap_false.

            LOOP AT c_th_data ASSIGNING FIELD-SYMBOL(<fs_data>).

              CLEAR: l_status.

              " --- Leer campos necesarios del registro actual ---
              DATA(lv_user_row)      = VALUE /bi0/oiusername( ).
              DATA(lv_acc_row)       = VALUE /bi0/oigl_account( ).
              DATA(lv_ceco_row)      = VALUE /bi0/oicostcenter( ).
              DATA(lv_orden_row)     = VALUE /bi0/oicoorder( ).
              DATA(lv_infoprov_row)  = VALUE rsinfoprov( ).
              DATA(lv_year_row)      = VALUE /bi0/oifiscyear( ).
              DATA(lv_ver_row)       = VALUE /bi0/oiversion( ).
              DATA(lv_tabla_row)     = VALUE /bic/oiztabla( ).
              DATA(lv_comp_code)     = VALUE /bi0/oicomp_code( ).
              DATA(lv_co_area)       = VALUE /bi0/oico_area( ).

              ASSIGN COMPONENT '0USERNAME'   OF STRUCTURE <fs_data> TO <f>.
              IF <f> IS ASSIGNED. lv_user_row     = <f>. ENDIF.
              ASSIGN COMPONENT '0GL_ACCOUNT' OF STRUCTURE <fs_data> TO <f>.
              IF <f> IS ASSIGNED. lv_acc_row      = <f>. ENDIF.
              ASSIGN COMPONENT '0COSTCENTER' OF STRUCTURE <fs_data> TO <f>.
              IF <f> IS ASSIGNED. lv_ceco_row     = <f>. ENDIF.
              ASSIGN COMPONENT '0COORDER'    OF STRUCTURE <fs_data> TO <f>.
              IF <f> IS ASSIGNED. lv_orden_row    = <f>. ENDIF.
              ASSIGN COMPONENT '0INFOPROV'   OF STRUCTURE <fs_data> TO <f>.
              IF <f> IS ASSIGNED. lv_infoprov_row = <f>. ENDIF.
              ASSIGN COMPONENT '0FISCYEAR'   OF STRUCTURE <fs_data> TO <f>.
              IF <f> IS ASSIGNED. lv_year_row     = <f>. ENDIF.
              ASSIGN COMPONENT '0VERSION'    OF STRUCTURE <fs_data> TO <f>.
              IF <f> IS ASSIGNED. lv_ver_row      = <f>. ENDIF.
              ASSIGN COMPONENT 'ZTABLA'      OF STRUCTURE <fs_data> TO <f>.
              IF <f> IS ASSIGNED. lv_tabla_row    = <f>. ENDIF.
              ASSIGN COMPONENT 'ZPL_DUM01'   OF STRUCTURE <fs_data> TO <f>.
              IF <f> IS ASSIGNED. l_status        = <f>. ENDIF.
              ASSIGN COMPONENT '0COMP_CODE'      OF STRUCTURE <fs_data> TO <f>.
              IF <f> IS ASSIGNED. lv_comp_code    = <f>. ENDIF.
              ASSIGN COMPONENT '0CO_AREA'   OF STRUCTURE <fs_data> TO <f>.
              IF <f> IS ASSIGNED. lv_co_area        = <f>. ENDIF.



              " --- La combinación relevante del “destino” (según parámetros y l_infoprov) ---
              DATA(lv_combo_match) = abap_false.
              IF     lv_acc_row     = wa_clave-gl_account
                 AND lv_ceco_row    = wa_clave-costcenter
                 AND lv_orden_row   = wa_clave-coorder
                 AND lv_comp_code   = wa_clave-comp_code
                 AND lv_co_area     = wa_clave-co_area
*                 AND lv_infoprov_row = l_infoprov
                 AND lv_year_row    = l_fiscyear
                 AND lv_ver_row     = l_version
                 AND ( lv_tabla_row IS INITIAL OR lv_tabla_row = zpl_utiles=>c_plan ).
                lv_combo_match = abap_true.
              ENDIF.

              IF lv_combo_match EQ abap_true.
                lv_combo_found = abap_true.

                " Caso 2 (parte 1): si ya existe la línea del usuario destino, marcar como activa
                IF lv_user_row = wa_clave-username and lv_infoprov_row eq l_infoprov.
                  lv_new_exists = abap_true.
                  " Asegurar status 1 para la combinación del usuario destino
                  ASSIGN COMPONENT 'ZPL_DUM01' OF STRUCTURE <fs_data> TO <o>.
                  IF <o> IS ASSIGNED. <o> = 1. UNASSIGN <o>. ENDIF.
                ELSE.
                  " Caso 2 y 3) Si hay otra activa de otro usuario, la anotamos
                  lv_old_active_exists = abap_true.
                  " En caso 3, se normaliza a 0 aquí mismo (si ya existe el destino)
                  IF lv_new_exists EQ abap_true.
                    ASSIGN COMPONENT 'ZPL_DUM01' OF STRUCTURE <fs_data> TO <o>.
                    IF <o> IS ASSIGNED. <o> = 0. UNASSIGN <o>. ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            ENDLOOP.

            " --- Decisiones post-escaneo ---
            " CASO 2: Existe asignada a otro usuario activo o debe "moverse", pero NO existe la del destino -> clonar UNA VEZ por cada período existente
            IF lv_combo_found EQ abap_true AND lv_old_active_exists EQ abap_true AND lv_new_exists EQ abap_false.

              LOOP AT c_th_data ASSIGNING FIELD-SYMBOL(<fs_src>).
                " Repetimos el match para quedarnos con las filas (p.ej. 12 períodos)
                DATA(lv_match_src) = abap_false.

                ASSIGN COMPONENT '0COMP_CODE'  OF STRUCTURE <fs_src> TO <f>.
                IF <f> IS ASSIGNED AND <f> = wa_clave-comp_code. ELSE. CONTINUE. ENDIF.
                ASSIGN COMPONENT '0CO_AREA' OF STRUCTURE <fs_src> TO <f>.
                IF <f> IS ASSIGNED AND <f> = wa_clave-co_area.  ELSE. CONTINUE. ENDIF.

                ASSIGN COMPONENT '0GL_ACCOUNT' OF STRUCTURE <fs_src> TO <f>.
                IF <f> IS ASSIGNED AND <f> = wa_clave-gl_account. ELSE. CONTINUE. ENDIF.
                ASSIGN COMPONENT '0COSTCENTER' OF STRUCTURE <fs_src> TO <f>.
                IF <f> IS ASSIGNED AND <f> = wa_clave-costcenter.  ELSE. CONTINUE. ENDIF.
                ASSIGN COMPONENT '0COORDER'    OF STRUCTURE <fs_src> TO <f>.
                IF <f> IS ASSIGNED AND <f> = wa_clave-coorder.    ELSE. CONTINUE. ENDIF.
*                ASSIGN COMPONENT '0INFOPROV'   OF STRUCTURE <fs_src> TO <f>.
*                IF <f> IS ASSIGNED AND <f> eq l_infoprov. ELSE. CONTINUE. ENDIF.
                ASSIGN COMPONENT '0FISCYEAR'   OF STRUCTURE <fs_src> TO <f>.
                IF <f> IS ASSIGNED AND <f> = l_fiscyear.          ELSE. CONTINUE. ENDIF.
                ASSIGN COMPONENT '0VERSION'    OF STRUCTURE <fs_src> TO <f>.
                IF <f> IS ASSIGNED AND <f> = l_version.           ELSE. CONTINUE. ENDIF.
                lv_match_src = abap_true.

                IF lv_match_src eq abap_true.
                  " Clonar estructura
                  CREATE DATA lr_data LIKE LINE OF c_th_data.
                  ASSIGN lr_data->* TO <s>.

*                 Crea nuevo registro a partir del actual
*------------------------------------------------------------------------------------
                  LOOP AT <fs_fields> INTO wa_field."Recorre campos el Inforprov destino
                    ASSIGN COMPONENT wa_field-in OF STRUCTURE <fs_src> TO <f>.
                    IF <f> IS NOT ASSIGNED. CONTINUE. ENDIF.

                    CASE wa_field-in.
                      WHEN '0USERNAME'.
                        ASSIGN COMPONENT '0USERNAME' OF STRUCTURE <s> TO <o>.
                        IF <o> IS ASSIGNED. <o> = wa_clave-username. UNASSIGN <o>. ENDIF.

                      WHEN 'ZPL_DUM01'.
                        ASSIGN COMPONENT 'ZPL_DUM01' OF STRUCTURE <s> TO <o>.
                        IF <o> IS ASSIGNED. <o> = 1. UNASSIGN <o>. ENDIF.

                      WHEN '0INFOPROV'.
                        ASSIGN COMPONENT '0INFOPROV' OF STRUCTURE <s> TO <o>.
                        IF <o> IS ASSIGNED. <o> = l_infoprov. UNASSIGN <o>. ENDIF.

                      WHEN '0C0_AREA'.
                        ASSIGN COMPONENT '0CO_AREA' OF STRUCTURE <s> TO <o>.
                        IF <o> IS ASSIGNED. <o> = wa_clave-co_area. UNASSIGN <o>. ENDIF.


                      WHEN OTHERS.
                        ASSIGN COMPONENT wa_field-in OF STRUCTURE <s> TO <o>.
                        IF <o> IS ASSIGNED. <o> = <f>. UNASSIGN: <o>. ENDIF.
                    ENDCASE.
                  ENDLOOP.

                  INSERT <s> INTO TABLE <fs_new_data>.

                  " Desactivar la fuente si estaba activa
                  ASSIGN COMPONENT 'ZPL_DUM01' OF STRUCTURE <fs_src> TO <o>.
                  IF <o> IS ASSIGNED. <o> = 0. UNASSIGN <o>. ENDIF.

                ENDIF.
              ENDLOOP.

              INSERT LINES OF <fs_new_data> INTO TABLE c_th_data.
              FREE <fs_new_data>.

              " CASO 3: Existen ambas -> sólo normalizar estados (ya se hizo en el primer loop):
            ELSEIF lv_combo_found = abap_true AND lv_new_exists = abap_true.
              " Nada que insertar. Estados ya acomodados:
              " - Para el usuario destino: ZPL_DUM01 = 1
              " - Para otros usuarios:     ZPL_DUM01 = 0 cuando estaban activos

            ENDIF.

          ELSE. " c_th_data[] IS INITIAL => Se debe crear


            " Caso 1) Si la clave no existe, debe crearse:
            IF wa_clave IS NOT INITIAL AND l_crear = 1.

              DO 12 TIMES.

                CREATE DATA lr_data LIKE LINE OF c_th_data.
                ASSIGN lr_data->* TO <s>.

                LOOP AT <fs_fields> INTO wa_field.
                  ASSIGN COMPONENT wa_field-in OF STRUCTURE <s> TO <f>.
                  IF <f> IS NOT ASSIGNED.
                    CONTINUE.
                  ENDIF.

                  CASE wa_field-in.
                    WHEN '0INFOPROV'.
                      <f> = l_infoprov.
                      UNASSIGN <f>.

                    WHEN '0FISCPER'.
                      " Use the loop index as the fiscal period in the format 2024001 to 2024012
                      l_periodo = sy-index.
                      CONCATENATE l_fiscyear l_periodo INTO l_fiscper.
                      <f> = l_fiscper.
                      UNASSIGN <f>.

                    WHEN '0CHRT_ACCTS'.
                      <f> = 'SQS'.
                      UNASSIGN <f>.

                    WHEN '0FISCVARNT'.
                      <f> = 'K4'.
                      UNASSIGN <f>.

                    WHEN '0FISCYEAR'.
                      <f> = l_fiscyear.
                      UNASSIGN <f>.

                    WHEN '0VERSION'.
                      <f> = l_version.
                      UNASSIGN <f>.

                    WHEN 'ZTABLA'.
                      <f> = zpl_utiles=>c_plan.
                      UNASSIGN <f>.

                    WHEN '0CURRENCY'.
                      <f> = zpl_utiles=>c_pyg.
                      UNASSIGN <f>.

                    WHEN '0UNIT'.
                      <f> = zpl_utiles=>c_unit.
                      UNASSIGN <f>.

                    WHEN '0CURTYPE'.
                      <f> = zpl_utiles=>c_curtype.
                      UNASSIGN <f>.

                    WHEN 'ZPL_DUM01'.
                      <f> = 1.
                      UNASSIGN <f>.

                    WHEN 'ZPL_PGRAL'.
                      <f> = 1.
                      UNASSIGN <f>.


                    WHEN OTHERS.
                      ASSIGN COMPONENT wa_field-out OF STRUCTURE wa_clave TO <o>.
                      IF <o> IS ASSIGNED AND <f> IS ASSIGNED.
                        <f> = <o>.
                        UNASSIGN: <f>, <o>.
                      ENDIF.
                  ENDCASE.
                ENDLOOP.

                INSERT <s> INTO TABLE <fs_new_data>.
              ENDDO.

*            IF <fs_new_data> IS ASSIGNED.
*              REFRESH c_th_data[].
              INSERT LINES OF <fs_new_data> INTO TABLE c_th_data.
              FREE <fs_new_data>.
*            ENDIF.


            ENDIF. "Clave nueva
          ENDIF."Clave Válida
        ENDIF.


      CATCH cx_root INTO DATA(l_excepcion). " TODO: variable is assigned but never used (ABAP cleaner)
        MESSAGE i004(zpl) INTO l_dummy.
        i_r_msg->add_msg( ).

    ENDTRY.
  ENDMETHOD.


  METHOD if_rsplfa_srvtype_imp_exec~finish_execution.


  ENDMETHOD.


  METHOD if_rsplfa_srvtype_imp_exec~init_execution.


  ENDMETHOD.
ENDCLASS.
