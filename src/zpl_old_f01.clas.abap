CLASS zpl_old_f01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rsplfa_srvtype_imp_exec .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpl_old_f01 IMPLEMENTATION.


  METHOD if_rsplfa_srvtype_imp_exec~init_execution.


  ENDMETHOD.

  METHOD if_rsplfa_srvtype_imp_exec~execute.


    DATA: lv_zpl_link   TYPE /bic/oizpl_link,
          lv_event      TYPE char20,
          lv_source     TYPE /bi0/oiasource,
          lr_param_elem TYPE REF TO if_rsplfa_param_elem.
    DATA l_msg          TYPE if_rspls_cr_types=>tn_s_mesg.
    DATA: lv_chain    TYPE rspc_chain,
          lv_logid    TYPE rspc_logid,
          lv_instance TYPE rspc_instance.



    TRY.



        " Obtain the ZPL_LINK_M parameter value
        lr_param_elem = i_r_param_set->get_param_elem( 'ZPL_LINK' ).
        lr_param_elem->get_value( IMPORTING e_value = lv_zpl_link ).

        " Determine the event to trigger based on the parameter value
        CASE lv_zpl_link.
          WHEN 'PXQ'.
            lv_event = 'ZPL_PXQ'.
            lv_chain = 'ZPL_PC_PXQ_01'. " Replace with your process chain name
            lv_source = 'ZPL_PXQ_F14'.
          WHEN 'VAR'.
            lv_event = 'ZPL_VAR'.
            lv_chain = 'ZPL_PC_VAR_01_INIT'. " Replace with your process chain name
            lv_chain = 'ZPL_VAR_F02'.
          WHEN 'VARQ'.
            lv_event = 'ZPL_VARQ'.
            lv_chain = 'ZPL_PC_VARQ_01'. " Replace with your process chain name
            lv_source = 'ZPL_VARQ_F05'.
          WHEN OTHERS.
            RAISE EXCEPTION TYPE cx_rspm_process_not_found.

        ENDCASE.


        DATA: lt_data TYPE TABLE OF /bic/azpl_a851,  " The aDSO ZPL_A85 defined for storing data
              wa_data TYPE /bic/azpl_a851.

        " Fill the work area with the required data based on the scripted view logic
        wa_data-username = sy-uname.
        wa_data-date0 = sy-datum.
        wa_data-/bic/zpl_link = lv_zpl_link.
        wa_data-time = sy-timlo.  " Loading timestamp into 0time
        wa_data-/bic/ztabla = zpl_utiles=>c_plan.
        wa_data-version = zpl_utiles=>c_version.

        " Insert the work area data into the aDSO
        APPEND wa_data TO lt_data.
        INSERT /bic/azpl_a851 FROM TABLE lt_data.




        DATA: lt_error TYPE  rspc_t_variante.




        CALL FUNCTION 'RSPC_API_CHAIN_START' DESTINATION 'SMCLNT300'
          EXPORTING
            i_chain            = lv_chain
            i_synchron         = 'X'  " Execute synchronously, remove if asynchronous is needed
          IMPORTING
            e_logid            = lv_logid
            e_instance         = lv_instance
            e_t_process_failed = lt_error
          EXCEPTIONS
            invalid_chain      = 1
            no_start           = 2
            OTHERS             = 3.

        IF sy-subrc <> 0.
          " Handle error
        ENDIF.

*        " Trigger the event
*        CALL FUNCTION 'BP_EVENT_RAISE'
*          EXPORTING
*            eventid                = lv_event
**           eventparm              = space
**           target_instance        = space
**           target_mode            = space
*          EXCEPTIONS
*            bad_eventid            = 1
*            eventid_does_not_exist = 2
*            eventid_missing        = 3
*            raise_failed           = 4
*            OTHERS                 = 5.
*
*        IF sy-subrc <> 0.
*         RAISE EXCEPTION TYPE cx_event_not_coupled.
*        ENDIF.


      CATCH cx_root INTO DATA(l_excepcion). " TODO: variable is assigned but never used (ABAP cleaner)
        MESSAGE i004(zpl) INTO DATA(l_dummy).
        i_r_msg->add_msg( ).

    ENDTRY.
  ENDMETHOD.

  METHOD if_rsplfa_srvtype_imp_exec~finish_execution.


  ENDMETHOD.
ENDCLASS.
