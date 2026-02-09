CLASS zpl_pxq_f05 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rsplfa_srvtype_imp_exec .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpl_pxq_f05 IMPLEMENTATION.


  METHOD if_rsplfa_srvtype_imp_exec~init_execution.


  ENDMETHOD.

  METHOD if_rsplfa_srvtype_imp_exec~execute.
    DATA lo_utiles      TYPE REF TO zpl_utiles.
    DATA wa_clave       TYPE zpl_utiles=>ty_wa_clave.
    DATA lr_struc       TYPE REF TO data.
    DATA lr_tab         TYPE REF TO data.
    DATA l_r_param_elem TYPE REF TO if_rsplfa_param_elem.
    DATA l_msg          TYPE if_rspls_cr_types=>tn_s_mesg.
    DATA l_fiscyear     TYPE /bi0/oifiscyear.
    DATA l_fiscper      TYPE /bi0/oifiscper.
    DATA l_periodo      TYPE numc3.
    DATA l_infoprov     TYPE rsinfoprov.
    DATA: BEGIN OF wa_field,
            in  TYPE char12,
            out TYPE char12,
          END OF wa_field.

    FIELD-SYMBOLS <s>           TYPE any.
    FIELD-SYMBOLS <f>           TYPE any.
    FIELD-SYMBOLS <o>           TYPE any.
    FIELD-SYMBOLS <fs_new_data> TYPE HASHED TABLE.
    FIELD-SYMBOLS <fs_fields>   TYPE STANDARD TABLE.

    TRY.
        lo_utiles = NEW zpl_utiles( ).

        " Process all selected lines
        CREATE DATA lr_tab LIKE c_th_data.
        ASSIGN lr_tab->* TO <fs_new_data>.

        ASSIGN lr_tab->* TO <s>.

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

        l_r_param_elem = i_r_param_set->get_param_elem( '0INFOPROV' ).
        l_r_param_elem->get_value( IMPORTING e_value = l_infoprov  ).

        FREE l_r_param_elem.

        " Validate the key using zpl_utiles->check_clave
        lo_utiles->check_clave( EXPORTING i_soc    = wa_clave-comp_code
                                          i_soco   = wa_clave-co_area
                                          i_cuenta = wa_clave-gl_account
                                          i_cebe   = wa_clave-costcenter
                                          i_orden  = wa_clave-coorder
                                          i_user   = wa_clave-username
                                IMPORTING o_msg    = l_msg ).

        " Check the validation result (assuming o_msg indicates success or failure)
        IF l_msg-msgty = 'E'.
          " Handle validation failure by adding a message to i_r_msg

          " TODO: variable is assigned but never used; add pragma ##NEEDED (ABAP cleaner)
          MESSAGE e005(zpl) INTO DATA(l_dummy). "
          i_r_msg->add_msg( ).
        ELSE.


          IF sy-batch IS INITIAL.
            BREAK-POINT.
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


          " 2) Si la combinación ya existe, modifica el status de la combinacion previa a ZPL_DUMMY = 0
          CONCATENATE '/bic/a' l_infoprov '7' INTO DATA(l_repositorio).

          FIELD-SYMBOLS <wa_a0xx> TYPE any.
          FIELD-SYMBOLS <lt_a0xx> TYPE ANY TABLE.

          DATA lr_data TYPE REF TO data.

          " Create a dynamic work area based on the repository structure
          CREATE DATA lr_data TYPE TABLE OF (l_repositorio).
          ASSIGN lr_data->* TO <lt_a0xx>.

          SELECT * INTO TABLE @<lt_a0xx>
            FROM (l_repositorio)
            WHERE comp_code   = @wa_clave-comp_code
              AND co_area     = @wa_clave-co_area
              AND gl_account  = @wa_clave-gl_account
              AND costcenter  = @wa_clave-costcenter
              AND coorder     = @wa_clave-coorder
              AND username   <> @wa_clave-username.

          IF sy-subrc = 0.
            LOOP AT <lt_a0xx> ASSIGNING <wa_a0xx>.

              CREATE DATA lr_struc LIKE LINE OF c_th_data.
              ASSIGN lr_struc->* TO <s>.

              LOOP AT <fs_fields> INTO wa_field.
                ASSIGN COMPONENT wa_field-out OF STRUCTURE <s> TO <f>.
                IF <f> IS NOT ASSIGNED.
                  CONTINUE.
                ENDIF.

                CASE wa_field-out.
                  WHEN '0INFOPROV'.
                    <f> = l_infoprov.
                    UNASSIGN <f>.
*
*                WHEN '0CHRT_ACCTS'.
*                  <f> = 'SQS'.
*
*                WHEN '0FISCVARNT'.
*                  <f> = 'K4'.
*
*                WHEN '0FISCYEAR'.
*                  <f> = l_fiscyear.

                  WHEN 'ZPL_DUM01'.
                    <f> = 0.
                    UNASSIGN <f>.

                  WHEN OTHERS.
                    ASSIGN COMPONENT wa_field-in OF STRUCTURE <wa_a0xx> TO <o>.
                    IF <o> IS ASSIGNED.
                      <f> = <o>.
                      UNASSIGN: <f>, <o>.
                    ENDIF.
                ENDCASE.
              ENDLOOP.

              INSERT <s> INTO TABLE <fs_new_data>.
            ENDLOOP.
          ENDIF.

          " 3) Si la clave no existe, debe crearse:
          IF wa_clave IS NOT INITIAL.

*            DO 12 TIMES.

            CREATE DATA lr_struc LIKE LINE OF c_th_data.
            ASSIGN lr_struc->* TO <s>.

            LOOP AT <fs_fields> INTO wa_field.
              ASSIGN COMPONENT wa_field-out OF STRUCTURE <s> TO <f>.
              IF <f> IS NOT ASSIGNED.
                CONTINUE.
              ENDIF.

              CASE wa_field-out.
                WHEN '0INFOPROV'.
                  <f> = l_infoprov.
                  UNASSIGN <f>.

*                  WHEN '0FISCPER'.
*                    " Use the loop index as the fiscal period in the format 2024001 to 2024012
*                    l_periodo = sy-index.
*                    CONCATENATE l_fiscyear l_periodo INTO l_fiscper.
*                    <f> = l_fiscper.
*                    UNASSIGN <f>.

                WHEN '0CHRT_ACCTS'.
                  <f> = 'SQS'.
                  UNASSIGN <f>.

                WHEN '0FISCVARNT'.
                  <f> = 'K4'.
                  UNASSIGN <f>.

                WHEN '0FISCYEAR'.
                  <f> = l_fiscyear.
                  UNASSIGN <f>.

                WHEN 'ZPL_DUM01'.
                  <f> = 1.
                  UNASSIGN <f>.

                WHEN OTHERS.
                  ASSIGN COMPONENT wa_field-in OF STRUCTURE wa_clave TO <o>.
                  IF <o> IS ASSIGNED.
                    <f> = <o>.
                    UNASSIGN: <f>, <o>.
                  ENDIF.
              ENDCASE.
            ENDLOOP.

            INSERT <s> INTO TABLE <fs_new_data>.
*            ENDDO.
          ENDIF.

          IF <fs_new_data> IS ASSIGNED.
            REFRESH c_th_data[].
            INSERT LINES OF <fs_new_data> INTO TABLE c_th_data.
          ENDIF.
        ENDIF.

      CATCH cx_root INTO DATA(l_excepcion). " TODO: variable is assigned but never used (ABAP cleaner)
        MESSAGE i004(zpl) INTO l_dummy.
        i_r_msg->add_msg( ).

    ENDTRY.
  ENDMETHOD.

  METHOD if_rsplfa_srvtype_imp_exec~finish_execution.


  ENDMETHOD.
ENDCLASS.
