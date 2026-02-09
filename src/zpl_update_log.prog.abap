*&---------------------------------------------------------------------*
*& Report ZPL_UPDATE_LOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPL_UPDATE_LOG.



        DATA: lt_data TYPE TABLE OF /bic/azpl_a851,  " The aDSO ZPL_A85 defined for storing data
              wa_data TYPE /bic/azpl_a851,
              l_link type /bic/oizpl_link.

        " Fill the work area with the required data based on the scripted view logic
        wa_data-username = sy-uname.
        wa_data-date0 = sy-datum.
        wa_data-/bic/zpl_link = l_link.
        wa_data-time = sy-timlo.  " Loading timestamp into 0time
        wa_data-/bic/ztabla = zpl_utiles=>c_plan.
        wa_data-version = zpl_utiles=>c_version.


BREAK-POINT.

        " Insert the work area data into the aDSO
        APPEND wa_data TO lt_data.
        INSERT /bic/azpl_a851 FROM TABLE lt_data.
