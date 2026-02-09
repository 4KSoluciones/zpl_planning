CLASS zpl_var_f06 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rsplfa_srvtype_imp_exec .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpl_var_f06 IMPLEMENTATION.





  METHOD if_rsplfa_srvtype_imp_exec~execute.


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




    TRY.

        lo_utiles = NEW zpl_utiles( ).


*        comp_code      TYPE /bi0/oicomp_code,
*        co_area        TYPE /bi0/oico_area,
*        username       TYPE /bi0/oiusername,
*        fiscper        TYPE /bi0/oifiscper,
*
*        gl_account     TYPE /bi0/oigl_account,
*        costcenter     TYPE /bi0/oicostcenter,
*        coorder        TYPE /bi0/oicoorder,

        LOOP AT c_th_data ASSIGNING <s>.

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

          ASSIGN COMPONENT '0USERNAME' OF STRUCTURE <s> TO <f>.
          IF <f> IS ASSIGNED.
            wa_clave-username = <f>.
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

            SORT lt_result BY comp_code co_area username fiscper gl_account costcenter coorder.

            LOOP AT c_th_data ASSIGNING <l>.

              ASSIGN COMPONENT 'ZPLCOMPCO' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-comp_code = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0CO_AREA' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-co_area = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0USERNAME' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-username = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0FISCPER' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-fiscper = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0GL_ACCOUNT' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-gl_account = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0COSTCENTER' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-costcenter = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0COORDER' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-coorder = <f>.
                UNASSIGN <f>.
              ENDIF.


*          CHECK l_status ne 0.
              READ TABLE lt_result ASSIGNING FIELD-SYMBOL(<fs_result>) WITH KEY comp_code  = wa_result-comp_code
                                                                                co_area    =  wa_result-co_area
                                                                                username   = wa_result-username
                                                                                fiscper    = wa_result-fiscper
                                                                                gl_account = wa_result-gl_account
                                                                                costcenter = wa_result-costcenter
                                                                                coorder    = wa_result-coorder BINARY SEARCH.
              IF sy-subrc EQ 0.
                ASSIGN COMPONENT 'ZPL_VENTP' OF STRUCTURE <l> TO <f>.
                IF <f> IS ASSIGNED.
                  <f> = <fs_result>-/bic/zpl_ventp.
                  UNASSIGN <f>.
                ENDIF.

              ENDIF.
              CLEAR:  wa_result.

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

          SORT lt_result BY comp_code co_area username fiscper gl_account costcenter coorder.

            LOOP AT c_th_data ASSIGNING <l>.

              ASSIGN COMPONENT 'ZPLCOMPCO' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-comp_code = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0CO_AREA' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-co_area = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0USERNAME' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-username = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0FISCPER' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-fiscper = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0GL_ACCOUNT' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-gl_account = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0COSTCENTER' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-costcenter = <f>.
                UNASSIGN <f>.
              ENDIF.

              ASSIGN COMPONENT '0COORDER' OF STRUCTURE <l> TO <f>.
              IF <f> IS ASSIGNED.
                wa_result-coorder = <f>.
                UNASSIGN <f>.
              ENDIF.


*          CHECK l_status ne 0.
              READ TABLE lt_result ASSIGNING <fs_result> WITH KEY comp_code  = wa_result-comp_code
                                                                                co_area    =  wa_result-co_area
                                                                                username   = wa_result-username
                                                                                fiscper    = wa_result-fiscper
                                                                                gl_account = wa_result-gl_account
                                                                                costcenter = wa_result-costcenter
                                                                                coorder    = wa_result-coorder BINARY SEARCH.
              IF sy-subrc EQ 0.
                ASSIGN COMPONENT 'ZPL_VENTP' OF STRUCTURE <L> TO <f>.
                IF <f> IS ASSIGNED.
                  <f> = <fs_result>-/bic/zpl_ventp.
                  UNASSIGN <f>.
                ENDIF.

              ENDIF.
              CLEAR:  wa_result.

            ENDLOOP.
          ENDIF.


        ENDIF."lt_claves[] is initial
      CATCH  cx_root INTO DATA(l_excepcion).
    ENDTRY.
  ENDMETHOD.

  METHOD if_rsplfa_srvtype_imp_exec~finish_execution.

  ENDMETHOD.

  METHOD if_rsplfa_srvtype_imp_exec~init_execution.

  ENDMETHOD.

ENDCLASS.
