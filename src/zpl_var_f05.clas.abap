CLASS zpl_var_f05 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rsplfa_srvtype_imp_exec .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpl_var_f05 IMPLEMENTATION.


  METHOD if_rsplfa_srvtype_imp_exec~init_execution.


  ENDMETHOD.

  METHOD if_rsplfa_srvtype_imp_exec~execute.
    DATA lo_utiles      TYPE REF TO zpl_utiles.
    DATA wa_clave       TYPE zpl_utiles=>ty_wa_clave.
    DATA lr_data        TYPE REF TO data.
    DATA lr_tab         TYPE REF TO data.
    DATA l_r_param_elem TYPE REF TO if_rsplfa_param_elem.
    DATA l_msg          TYPE if_rspls_cr_types=>tn_s_mesg.
    DATA l_coef         TYPE /bic/oizpl_coef.
    DATA l_cofin        TYPE /bic/oizpl_cofin.
    DATA l_part         TYPE /bic/oizpl_part.
    DATA l_venta        TYPE /bic/oizpl_venta.
    DATA l_gastp        TYPE /bic/oizpl_gastp.
    DATA l_ventp        TYPE /bic/oizpl_ventp.
    DATA l_gasto        TYPE /bic/oizpl_gasto.
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
        ASSIGN lo_utiles->gt_fields_var TO <fs_fields>.


        LOOP AT c_th_data ASSIGNING <fs_th_data>.
          CLEAR: l_coef, l_cofin, l_part, l_venta, l_ventp, l_gasto.

          LOOP AT <fs_fields> INTO wa_field.
            ASSIGN COMPONENT wa_field-in OF STRUCTURE <fs_th_data> TO <f>.
            IF <f> IS NOT ASSIGNED.
              CONTINUE.
            ENDIF.

            CASE wa_field-in.
              WHEN 'ZPL_COEF'.
                l_coef = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_COFIN'.
                l_cofin = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_PART'.
                l_part = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_VENTA'.
                l_venta = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_VENTP'.
                l_ventp = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_GASTO'. "Gasto Real
                l_gasto = <f>.
                UNASSIGN <f>.

            ENDCASE.
          ENDLOOP.


          ASSIGN COMPONENT 'ZPL_GASTP' OF STRUCTURE <fs_th_data> TO <f>.
          IF <f> IS ASSIGNED.

            IF l_part NE 0.
              l_ventp = l_ventp * l_part / 100 .
            ELSE.
              l_ventp = l_ventp.
            ENDIF.


            IF l_cofin NE 0.
              l_gastp = l_ventp *  l_cofin / 100.

            ELSE.
              l_gastp = l_ventp * l_coef.
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
