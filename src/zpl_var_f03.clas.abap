CLASS zpl_var_f03 DEFINITION "Parte de la funcion ZPL_VAR_TF03, Actualizacion de Vta Pptada.
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rsplfa_srvtype_imp_exec_ref .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpl_var_f03 IMPLEMENTATION.

  METHOD if_rsplfa_srvtype_imp_exec_ref~add_new_blocks.



    DATA: l_r_new_block TYPE REF TO data.

    FIELD-SYMBOLS: <s_ref_data>  TYPE any,
                   <s_new_block> TYPE any.

    CREATE DATA l_r_new_block LIKE LINE OF e_ts_new_blocks.
    ASSIGN l_r_new_block->* TO <s_new_block>.

    LOOP AT i_th_ref_data ASSIGNING <s_ref_data>.
      MOVE-CORRESPONDING <s_ref_data> TO <s_new_block>.
      READ TABLE i_ts_existing_blocks TRANSPORTING NO FIELDS FROM <s_new_block>.
      IF sy-subrc <> 0.
        COLLECT <s_new_block> INTO e_ts_new_blocks.
      ENDIF.
    ENDLOOP.



  ENDMETHOD.



  METHOD if_rsplfa_srvtype_imp_exec_ref~execute.


    DATA: l_version TYPE /bi0/oiversion,
          lo_utiles TYPE REF TO zpl_utiles.


    DATA: lr_struc        TYPE REF TO data,
          lr_tab          TYPE REF TO data,
          l_r_param_elem  TYPE REF TO if_rsplfa_param_elem,
          ls_data_charsel TYPE rsplf_s_charsel,
          wa_clave        TYPE zpl_utiles=>ty_wa_clave,
          lt_claves       TYPE zpl_utiles=>ty_lt_claves_aux , "No ordenada
          wa_result       TYPE zpl_utiles=>ty_wa_result_vta,
          lt_result       TYPE zpl_utiles=>ty_lt_result_vta,
          lr_result_2000  TYPE zpl_utiles=>ty_lt_result_vta,
          l_status        TYPE n LENGTH 1.



    FIELD-SYMBOLS: <l>       TYPE any,
                   <f>       TYPE any,
                   <s>       TYPE any,
                   <o>       TYPE any,
                   <r>       TYPE any,
                   <fs_data> TYPE HASHED TABLE.


    IF sy-batch IS INITIAL.
     BREAK-POINT.
    ENDIF.
    TRY.

        CHECK i_th_ref_data[] IS NOT INITIAL.
        lo_utiles = NEW zpl_utiles( ).

* Process all selected lines
        CREATE DATA lr_tab LIKE c_th_data.
        ASSIGN lr_tab->* TO <fs_data>.


        FREE l_r_param_elem.

        FREE c_th_data[].

        LOOP AT i_th_ref_data ASSIGNING <l>.

          CREATE DATA lr_struc LIKE <l>.
          ASSIGN lr_struc->* TO <s>.

          MOVE-CORRESPONDING <l> TO <s>.

          ASSIGN COMPONENT 'ZPLCOMPCO' OF STRUCTURE <s> TO <f>.
          IF <f> IS ASSIGNED.
            wa_clave-comp_code = <f>.
            UNASSIGN <f>.
          ENDIF.

          ASSIGN COMPONENT '0CO_AREA' OF STRUCTURE <s> TO <f>.
          IF <f> IS ASSIGNED.
            wa_clave-co_area = <f>.
            UNASSIGN <f>.
          ENDIF.

          ASSIGN COMPONENT '0GL_ACCOUNT' OF STRUCTURE <s> TO <f>.
          IF <f> IS ASSIGNED.
            wa_clave-gl_account = <f>.
            UNASSIGN <f>.
          ENDIF.

          ASSIGN COMPONENT '0COSTCENTER' OF STRUCTURE <s> TO <f>.
          IF <f> IS ASSIGNED.
            wa_clave-costcenter = <f>.
            UNASSIGN <f>.
          ENDIF.

          ASSIGN COMPONENT '0COORDER' OF STRUCTURE <s> TO <f>.
          IF <f> IS ASSIGNED.
            wa_clave-coorder = <f>.
            UNASSIGN <f>.
          ENDIF.


          ASSIGN COMPONENT '0USERNAME' OF STRUCTURE <s> TO <f>.
          IF <f> IS ASSIGNED.
            wa_clave-username = <f>.
            UNASSIGN <f>.
          ENDIF.

*
*          ASSIGN COMPONENT 'ZPL_DUM01' OF STRUCTURE <s> TO <f>.
*          IF <f> IS ASSIGNED.
*            l_status = <f>.
*            UNASSIGN <f>.
*          ENDIF.

*
*          CHECK l_status ne 0.
          APPEND wa_clave TO lt_claves.
          CLEAR:  wa_clave, l_status.

        ENDLOOP.

        IF lt_claves[] IS INITIAL.
          MESSAGE i001(zpl) INTO DATA(l_dummy).
          i_r_msg->add_msg( ).

        ELSE.

          DATA(lt_claves_2000) = lt_claves[].
          DELETE lt_claves_2000 WHERE comp_code NE '2000'.
          DELETE lt_claves WHERE comp_code EQ '2000'.



*1) Calcula venta Presupuestada para Sociedades <> 20000
          IF lt_claves[] IS NOT INITIAL.
            lo_utiles->get_vta_presup(
              EXPORTING
                i_claves = lt_claves
                i_tabla  = zpl_utiles=>c_plan
              IMPORTING
                e_result = lt_result
            ).

            DELETE  lt_result WHERE username IS INITIAL.

            LOOP AT lt_result ASSIGNING <r>.

              CREATE DATA lr_struc LIKE LINE OF c_th_data.
              ASSIGN lr_struc->* TO <s>.

              LOOP AT lo_utiles->gt_fields_var INTO DATA(wa_field).
                ASSIGN COMPONENT wa_field-in OF STRUCTURE <s> TO <f>.
                IF <f> IS ASSIGNED and sy-subrc eq 0.
                  CASE wa_field-in.
                    WHEN '0INFOPROV'.
                      <f> = 'ZPL_A041'.
*                    WHEN 'ZPL_DUM01'.
*                      <f> = 1.
                    WHEN '0VERSION'.
                      <f> = zpl_utiles=>c_version.
                    WHEN 'ZTABLA'.
                      <f> = zpl_utiles=>c_plan.

                    WHEN OTHERS.
                      ASSIGN COMPONENT wa_field-out OF STRUCTURE <r> TO <o>.
                      IF <o> IS ASSIGNED.
                        <f> = <o>.
                        UNASSIGN: <f>, <o>.
                      ENDIF.
                  ENDCASE.
                ENDIF.
              ENDLOOP.
              INSERT  <s> INTO TABLE  <fs_data>.
            ENDLOOP.
          ENDIF.

          CLEAR lt_result[].

*1) Calcula venta Presupuestada para Sociedad 20000
          IF lt_claves_2000[] IS NOT INITIAL.

            lo_utiles->get_vta_2000(
                 EXPORTING
                   i_claves = lt_claves_2000
                 IMPORTING
                   e_result = lt_result
               ).

            DELETE  lt_result WHERE username IS INITIAL.

            LOOP AT lt_result ASSIGNING <r>.

              CREATE DATA lr_struc LIKE LINE OF c_th_data.
              ASSIGN lr_struc->* TO <s>.

              LOOP AT lo_utiles->gt_fields_var INTO wa_field.
                ASSIGN COMPONENT wa_field-in OF STRUCTURE <s> TO <f>.
                   IF <f> IS ASSIGNED and sy-subrc eq 0.
                  CASE wa_field-in.
                    WHEN '0INFOPROV'.
                      <f> = 'ZPL_A041'.
*                    WHEN 'ZPL_DUM01'.
*                      <f> = 3.
                    WHEN '0VERSION'.
                      <f> = zpl_utiles=>c_version.
                    WHEN 'ZTABLA'.
                      <f> = zpl_utiles=>c_plan.

                    WHEN OTHERS.
                      ASSIGN COMPONENT wa_field-out OF STRUCTURE <r> TO <o>.
                      IF <o> IS ASSIGNED.
                        <f> = <o>.
                        UNASSIGN: <f>, <o>.
                      ENDIF.
                  ENDCASE.
                ENDIF.
              ENDLOOP.
              INSERT  <s> INTO TABLE  <fs_data>.
            ENDLOOP.
          ENDIF.
*3)   Inserta todas las líneas en el Result Set
          INSERT LINES OF  <fs_data> INTO TABLE  c_th_data.
        ENDIF.
      CATCH  cx_root INTO DATA(l_excepcion).
    ENDTRY.
  ENDMETHOD.



  METHOD if_rsplfa_srvtype_imp_exec_ref~get_ref_data_sel.


    DATA: ls_ref_charsel LIKE LINE OF e_t_ref_charsel.

*copy all restrictions:
    e_t_ref_charsel[] = i_t_data_charsel[].

    DELETE e_t_ref_charsel WHERE iobjnm = '0CURRENCY'.
    DELETE e_t_ref_charsel WHERE iobjnm = '0CURTYPE'.
    DELETE e_t_ref_charsel WHERE iobjnm = '0FISCYEAR'.
    DELETE e_t_ref_charsel WHERE iobjnm = '0INFOPROV'.
    DELETE e_t_ref_charsel WHERE iobjnm = '0UNIT'.
*    DELETE e_t_ref_charsel WHERE iobjnm = '0VERSION'.
*    DELETE e_t_ref_charsel WHERE iobjnm = 'ZTABLA'.
*    DELETE e_t_ref_charsel WHERE iobjnm = 'ZPLCOMPCO'.


*add infoprovider "ZPL_A03" to the reference data selection:
    ls_ref_charsel-iobjnm = '0INFOPROV'.

    ls_ref_charsel-sign = 'I'.
    ls_ref_charsel-opt = 'EQ'.
    ls_ref_charsel-low = 'ZPL_A041'.

    CLEAR ls_ref_charsel-high.
    APPEND ls_ref_charsel TO e_t_ref_charsel.





    .
*    delete e_t_ref_charsel where iobjnm = '0VERSION'.

    SORT e_t_ref_charsel.
    DELETE ADJACENT DUPLICATES FROM e_t_ref_charsel.


  ENDMETHOD.

  METHOD if_rsplfa_srvtype_imp_exec_ref~init_execution.
  ENDMETHOD.


  METHOD if_rsplfa_srvtype_imp_exec_ref~finish_execution.

  ENDMETHOD.

ENDCLASS.
