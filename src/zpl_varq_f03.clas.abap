CLASS zpl_varq_f03 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rsplfa_srvtype_imp_exec .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpl_varq_f03 IMPLEMENTATION.


  METHOD if_rsplfa_srvtype_imp_exec~init_execution.


  ENDMETHOD.

  METHOD if_rsplfa_srvtype_imp_exec~execute.
    DATA lo_utiles      TYPE REF TO zpl_utiles.
    DATA wa_clave       TYPE zpl_utiles=>ty_wa_clave.
    DATA lr_data        TYPE REF TO data.
    DATA lr_tab         TYPE REF TO data.
    DATA l_r_param_elem TYPE REF TO if_rsplfa_param_elem.
    DATA l_msg          TYPE if_rspls_cr_types=>tn_s_mesg.
    DATA l_vtaqp         TYPE /bic/oizpl_vtaqp.
    DATA l_qinpu        TYPE /bic/oizpl_qinpu.
    DATA l_username     TYPE /bi0/oiusername.
    DATA l_upric        TYPE /bic/oizpl_upric.
    DATA l_gastp        TYPE /bic/oizpl_gastp.

    DATA: BEGIN OF wa_field,
            in  TYPE char14,
            out TYPE char14,
          END OF wa_field.


    FIELD-SYMBOLS <f>           TYPE any.
    FIELD-SYMBOLS <fs_th_data>  TYPE any.
    FIELD-SYMBOLS <fs_fields>   TYPE STANDARD TABLE.


    TRY.
        lo_utiles = NEW zpl_utiles( ).



        " Assign the appropriate field symbol based on the value of l_infoprov
        ASSIGN lo_utiles->gt_fields_varq TO <fs_fields>.


        LOOP AT c_th_data ASSIGNING <fs_th_data>.
          CLEAR: l_vtaqp, l_qinpu, l_username, l_upric.

          LOOP AT <fs_fields> INTO wa_field.
            ASSIGN COMPONENT wa_field-in OF STRUCTURE <fs_th_data> TO <f>.
            IF <f> IS NOT ASSIGNED.
              CONTINUE.
            ENDIF.

            CASE wa_field-in.
              WHEN 'ZPL_VTAQP'.
                l_vtaqp = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_QINPU'.
                l_qinpu = <f>.
                UNASSIGN <f>.


              WHEN '0USERNAME'.
                l_username = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_UPRIC'.
                l_upric = <f>.
                UNASSIGN <f>.


            ENDCASE.
          ENDLOOP.


          ASSIGN COMPONENT 'ZPL_GASTP' OF STRUCTURE <fs_th_data> TO <f>.
          IF <f> IS ASSIGNED.


            "Si el usuario está dentro de las excepciones:
            IF l_username IN lo_utiles->gr_user_varq.
              IF l_qinpu IS INITIAL.
                l_gastp = l_vtaqp * l_upric.
              ELSE.
                l_gastp  = l_qinpu * l_upric.
              ENDIF.
            ELSE.

              l_gastp = l_qinpu * l_upric.

            ENDIF.

            <f> = l_gastp.

            UNASSIGN <f>.
          ELSE.
            CONTINUE.
          ENDIF.


        ENDLOOP.


      CATCH cx_root INTO DATA(l_excepcion). " TODO: variable is assigned but never used (ABAP cleaner)
        MESSAGE i004(zpl) INTO DATA(l_dummy).
        i_r_msg->add_msg( ).

    ENDTRY.
  ENDMETHOD.

  METHOD if_rsplfa_srvtype_imp_exec~finish_execution.


  ENDMETHOD.
ENDCLASS.
