CLASS zpl_pxq_f02 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rsplfa_srvtype_imp_exec .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpl_pxq_f02 IMPLEMENTATION.


  METHOD if_rsplfa_srvtype_imp_exec~init_execution.


  ENDMETHOD.

  METHOD if_rsplfa_srvtype_imp_exec~execute.
    DATA lo_utiles      TYPE REF TO zpl_utiles.
    DATA wa_clave       TYPE zpl_utiles=>ty_wa_clave.
    DATA lr_data        TYPE REF TO data.
    DATA lr_tab         TYPE REF TO data.
    DATA l_r_param_elem TYPE REF TO if_rsplfa_param_elem.
    DATA l_msg          TYPE if_rspls_cr_types=>tn_s_mesg.
    DATA l_qty          TYPE /bic/oizquantity.
    DATA l_pfila        TYPE /bic/oizpl_pfila.
    DATA l_pgral        TYPE /bic/oizpl_pgral.
    DATA l_upric        TYPE /bic/oizpl_upric.
    DATA l_gastp        TYPE /bic/oizpl_gastp.
    DATA l_qinpu        TYPE /bic/oizpl_qinpu.
    DATA l_gasto        TYPE /bic/oizpl_gasto.
    DATA: l_input       LIKE l_qinpu.
    DATA: l_comp_code   TYPE /bi0/oicomp_code.
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
        ASSIGN lo_utiles->gt_fields_pxq TO <fs_fields>.


        LOOP AT c_th_data ASSIGNING <fs_th_data>.
          CLEAR: l_qty, l_pfila, l_pgral, l_upric, l_gastp, l_gasto, l_qinpu.

          LOOP AT <fs_fields> INTO wa_field.
            ASSIGN COMPONENT wa_field-in OF STRUCTURE <fs_th_data> TO <f>.
            IF <f> IS NOT ASSIGNED.
              CONTINUE.
            ENDIF.


            CASE wa_field-in.
              WHEN 'ZPLCOMPCO'.
                l_comp_code = <f>.
                UNASSIGN <f>.

              WHEN 'ZQUANTITY'.
                l_qty = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_PFILA'.
                l_pfila = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_PGRAL'.
                l_pgral = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_UPRIC'.
                l_upric = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_QINPU'.
                l_qinpu = <f>.
                UNASSIGN <f>.


              WHEN 'ZPL_GASTO'. "Gasto Real
                l_gasto = <f>.
                UNASSIGN <f>.

            ENDCASE.
          ENDLOOP.


          ASSIGN COMPONENT 'ZPL_GASTP' OF STRUCTURE <fs_th_data> TO <f>.
          IF <f> IS ASSIGNED.

            IF l_upric GT 0 AND l_qinpu EQ 0 AND ( l_comp_code EQ '6000' OR l_comp_code EQ '7000' ).
              l_input = 1.
            ELSE.
              l_input = l_qinpu.
            ENDIF.
            l_gastp = l_upric *  l_input * ( 1 + l_pfila / 100 ).

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
