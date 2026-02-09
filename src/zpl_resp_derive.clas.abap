*--------------------------------------------------------------------*
*MC - 08/2023 - Derivación de Usuario Responsable, dada una clave Cuenta, CEBE, Orden
*---------------------------------------------------------------------*

CLASS zpl_resp_derive  DEFINITION
  PUBLIC
  CREATE PROTECTED .

*"* public components of class CL_RSPLS_CR_EXIT_BASE
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS abap .
    TYPE-POOLS rsd .

    INTERFACES if_rspls_char_relation .
    INTERFACES if_rspls_cr_exit .
    INTERFACES if_rspls_cr_methods .
    INTERFACES if_rspls_cr_types .

    ALIASES n_is_derive
      FOR if_rspls_char_relation~n_is_derive .
    ALIASES n_ts_chas
      FOR if_rspls_char_relation~n_ts_chas .

    DATA n_basic_prov TYPE rsinfoprov READ-ONLY .
    DATA n_step TYPE rspls_step READ-ONLY .
    DATA n_dateto TYPE rspls_cr_dateto READ-ONLY .





    INTERFACE if_rspls_cr_types LOAD .
    METHODS constructor
      IMPORTING
        !i_basic_prov  TYPE rsinfoprov
        !i_step        TYPE if_rspls_cr_types=>tn_step
        !i_is_derive   TYPE rs_bool
        !i_ts_cha_role TYPE if_rspls_cr_types=>tn_ts_cha_role
        !i_dateto      TYPE rspls_cr_dateto
      RAISING
        cx_rspls_failed .
  PROTECTED SECTION.
*"* protected components of class CL_RSPLS_CR_EXIT_BASE
*"* do not include other source files here!!!

    TYPES to_r_is_valid TYPE REF TO char1 .
    INTERFACE if_rspls_cr_types LOAD .
    TYPES to_r_mesg TYPE REF TO if_rspls_cr_types=>tn_s_mesg .

    DATA o_r_strucdescr TYPE REF TO cl_abap_structdescr .
    DATA o_r_s_buf TYPE REF TO data .
    DATA o_r_th_buf TYPE REF TO data .
    DATA o_r_th_buf_d TYPE REF TO data .
    DATA o_r_is_valid TYPE to_r_is_valid .
    DATA o_r_s_mesg TYPE to_r_mesg .
    DATA o_use_buffer TYPE rs_bool .

    DATA o_utiles TYPE REF TO zpl_utiles.

    CLASS-METHODS get_instance_o
      IMPORTING
        !i_basic_prov       TYPE rsinfoprov
        !i_step             TYPE if_rspls_cr_types=>tn_step
        !i_is_derive        TYPE rs_bool
        !i_ts_cha_role      TYPE if_rspls_cr_types=>tn_ts_cha_role
        !i_dateto           TYPE rspls_cr_dateto
        !i_clsname          TYPE seoclsname OPTIONAL
      RETURNING
        VALUE(r_r_instance) TYPE REF TO if_rspls_char_relation
      RAISING
        cx_rspls_failed .
    METHODS seldr_to_range
      IMPORTING
        !i_tsx_seldr TYPE rsdd_tsx_seldr
      EXPORTING
        !e_t_range   TYPE rsdri_t_range .
  PRIVATE SECTION.
*"* private components of class CL_RSPLS_CR_EXIT_BASE
*"* do not include other source files here!!!

    TYPES:
      BEGIN OF tp_sx_instance,
        basic_prov  TYPE rsinfoprov,
        ts_cha_role TYPE if_rspls_char_relation~tn_ts_cha_role,
        r_instance  TYPE REF TO if_rspls_char_relation,
      END OF tp_sx_instance .
    TYPES:
      tp_tsx_instance TYPE SORTED TABLE OF tp_sx_instance
                           WITH NON-UNIQUE KEY basic_prov
                           INITIAL SIZE 0 .

    CLASS-DATA p_tsx_instance TYPE tp_tsx_instance .
ENDCLASS.



CLASS zpl_resp_derive IMPLEMENTATION.


  METHOD constructor.
*---------------------------------------------------------------------*
*  --> i_basic_prov     basis infoprovider
*  --> i_step           step of characteristic relation
*  --> i_is_derive      flag, derive or only check combination
*  --> i_ts_cha_role    characteristics and their role
*  --> i_dateto         date: valid to ( or char. relationsships)
*---------------------------------------------------------------------*

    DATA:
      l_s_cha_role TYPE if_rspls_char_relation=>tn_s_cha_role,
      l_s_chas     TYPE if_rspls_char_relation=>tn_s_chas,
      l_r_cha_prop TYPE REF TO if_rsd_cha_prop,
      l_t_comp     TYPE cl_abap_structdescr=>component_table,
      l_s_comp     TYPE cl_abap_structdescr=>component,
      l_ts_chanm   TYPE rsd_ts_chanm,
      l_r_struc    TYPE REF TO cl_abap_structdescr,
      l_idx        TYPE i,
      l_ts_chanm_s TYPE rsd_ts_chanm.

    FIELD-SYMBOLS:
      <l_s_buf>  TYPE any,
      <l_char>   TYPE char1,
      <l_s_mesg> TYPE if_rspls_cr_types=>tn_s_mesg.

    if_rspls_char_relation~n_basic_prov = i_basic_prov.

    n_basic_prov = i_basic_prov.
    n_step       = i_step.
    n_is_derive  = i_is_derive.
    n_dateto     = i_dateto.

* fill internal view of characteristics for characteristic relationship
* and create a data structure containing these characteristics
    LOOP AT i_ts_cha_role INTO l_s_cha_role.
      l_s_chas-iobjnm  = l_s_cha_role-iobjnm.
      l_s_chas-role    = l_s_cha_role-role.
*   we want characteristic values in internal format
*   --> leave field 'is_sid' of l_s_chas empty
      l_r_cha_prop = cl_rsd_iobj_prop_cache=>get_cha( l_s_chas-iobjnm ).
*   l_r_cha_prop contains the meta data for the actual infoobject:
*   ATTENTION: method GET_FIELD_NAME contains the fieldname of BW
*   generated data structures, e.g. for reference characteristics
*   the names used in master data tables may be different !
      l_s_chas-fieldnm = l_r_cha_prop->get_field_name( ).
      INSERT l_s_chas INTO TABLE if_rspls_char_relation~n_ts_chas.
*   create the data structure at run time using the data element:
      l_s_comp-name = l_s_chas-fieldnm.
      l_s_comp-type = l_r_cha_prop->get_datadescr( ).
      APPEND l_s_comp TO l_t_comp.
*   fill key of buffer table:
      INSERT l_s_chas-fieldnm INTO TABLE l_ts_chanm.
      IF l_s_chas-role = 'S'.
        INSERT l_s_chas-fieldnm INTO TABLE l_ts_chanm_s.
      ENDIF.
    ENDLOOP.


    o_utiles = NEW zpl_utiles( ).

* now a type description of the data records can be created:
    o_r_strucdescr = cl_abap_structdescr=>get( p_components = l_t_comp
                                               p_strict     = abap_false ).
* create infrastructure to buffer the results of methods check/derive/create
    IF lines( l_ts_chanm ) > 0.
      l_idx    = lines( l_t_comp ).
      l_s_comp-name = '_IS_VALID'.
      l_s_comp-type = cl_abap_elemdescr=>get_c( 1 ).
      APPEND l_s_comp TO l_t_comp.
      ADD 1 TO l_idx.
      l_r_struc ?= cl_abap_structdescr=>describe_by_name( 'IF_RSPLS_CR_TYPES=>TN_S_MESG' ).
      l_s_comp-name = '_S_MESG'.
      l_s_comp-type = l_r_struc.
      APPEND l_s_comp TO l_t_comp.
      ADD 1 TO l_idx.
      l_r_struc = cl_abap_structdescr=>get( p_components = l_t_comp
                                            p_strict     = abap_false ).
      CREATE DATA o_r_s_buf TYPE HANDLE l_r_struc.
      ASSIGN o_r_s_buf->* TO <l_s_buf>.
      ASSIGN COMPONENT '_IS_VALID' OF STRUCTURE <l_s_buf> TO <l_char>.
*   the typed data reference points to the field '_IS_VALID'
      GET REFERENCE OF <l_char> INTO o_r_is_valid.
      ASSIGN COMPONENT l_idx OF STRUCTURE <l_s_buf> TO <l_s_mesg>.
*   the typed data reference points to the structure '_S_MESG'
      GET REFERENCE OF <l_s_mesg> INTO o_r_s_mesg.
      CREATE DATA o_r_th_buf LIKE HASHED TABLE OF <l_s_buf>
                             WITH UNIQUE KEY (l_ts_chanm).
      o_use_buffer = rs_c_true.
      IF n_is_derive = rs_c_true.
        CREATE DATA o_r_th_buf_d LIKE HASHED TABLE OF <l_s_buf>
                                 WITH UNIQUE KEY (l_ts_chanm_s).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance_o.
*---------------------------------------------------------------------*
*  --> i_basic_prov     basis infoprovider
*  --> i_step           step of characteristic relation
*  --> i_is_derive      flag, derive or only check combination
*  --> i_ts_cha_role    characteristics and their role
*  --> i_dateto         date: valid to ( or char. relationsships)
*  --> i_clsname        class name, used for subclasses
*  <-- r_r_instance     existing or new instance
*---------------------------------------------------------------------*

    DATA: l_exists      TYPE abap_bool,
          l_sx_instance TYPE tp_sx_instance,
          l_clsname     TYPE seoclsname,
          l_r_ex        TYPE REF TO cx_rspls_failed.

    FIELD-SYMBOLS: <l_sx_instance> TYPE tp_sx_instance.



* the instance table is sorted by basic_prov
* scan the relevant records by ts_cha_role
    l_exists = abap_false.
    LOOP AT p_tsx_instance ASSIGNING <l_sx_instance>
                           WHERE basic_prov = i_basic_prov.
      IF <l_sx_instance>-ts_cha_role = i_ts_cha_role.
        l_exists = abap_true.
        r_r_instance = <l_sx_instance>-r_instance.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF l_exists = abap_false.
      l_sx_instance-basic_prov  = i_basic_prov.
      l_sx_instance-ts_cha_role = i_ts_cha_role.

      TRY.

          IF i_clsname IS INITIAL.
            l_clsname = 'CL_RSPLS_CR_EXIT_BASE'.
          ELSE.
            l_clsname = i_clsname.
          ENDIF.

          CREATE OBJECT l_sx_instance-r_instance TYPE (l_clsname)
            EXPORTING
              i_basic_prov    = l_sx_instance-basic_prov
              i_step          = i_step
              i_is_derive     = i_is_derive
              i_ts_cha_role   = l_sx_instance-ts_cha_role
              i_dateto        = i_dateto.

        CATCH cx_rspls_failed INTO l_r_ex.
          RAISE EXCEPTION l_r_ex.
      ENDTRY.

      INSERT l_sx_instance INTO TABLE p_tsx_instance.
      r_r_instance = l_sx_instance-r_instance.

    ENDIF.

  ENDMETHOD.


  METHOD if_rspls_char_relation~create_data_ref.
*---------------------------------------------------------------------*
*  <-- r_r_chas  structure with infoobjects and fieldnames
*                as defined in table n_ts_chas
*---------------------------------------------------------------------*

    CREATE DATA r_r_chas TYPE HANDLE o_r_strucdescr.

  ENDMETHOD.


  METHOD if_rspls_char_relation~set_selection.

* I_TSX_SELDR contains information about the selection in the current
* context of a planning query or a planning function; this method can
* be helpful to prefetch data needed in the relation

* I_PREFETCH contains a hint that a prefetch of resources might
* improve performance, e.g. to a avoid a record based DB access


  ENDMETHOD.


  METHOD if_rspls_cr_exit~get_instance.
*---------------------------------------------------------------------*
*  --> i_basic_prov     basis infoprovider
*  --> i_step           step of characteristic relation
*  --> i_is_derive      flag, derive or only check combination
*  --> i_ts_cha_role    characteristics and their role
*  --> i_dateto         date: valid to ( or char. relationsships)
*  --> i_clsname        class name, used for subclasses
*  <-- r_r_instance     existing or new instance
*---------------------------------------------------------------------*

    DATA: l_r_ex TYPE REF TO cx_rspls_failed.

    TRY.

        CALL METHOD get_instance_o
          EXPORTING
            i_basic_prov  = i_basic_prov
            i_step        = i_step
            i_is_derive   = i_is_derive
            i_ts_cha_role = i_ts_cha_role
            i_dateto      = i_dateto
            i_clsname     = i_clsname
          RECEIVING
            r_r_instance  = r_r_instance.

      CATCH cx_rspls_failed INTO l_r_ex.
        RAISE EXCEPTION l_r_ex.
    ENDTRY.

  ENDMETHOD.


  METHOD if_rspls_cr_methods~check.
*---------------------------------------------------------------------*
*  --> i_s_chas    characteristics combination
*  <-- e_t_mesg    messages
*  <-- e_is_valid  flag, record is valid or not
*---------------------------------------------------------------------*

* begin example code:
* infrastructure needed by the buffer:
* Attention: The system also supports external buffering,
* check documentation of attribute N_USE_EXTERNAL_BUFFER
* and note 1067433
    DATA: l_s_mesg TYPE if_rspls_cr_types=>tn_s_mesg,
          l_user   TYPE char12.
*
    FIELD-SYMBOLS: <l_th_buf> TYPE HASHED TABLE,
                   <l_s_buf>  TYPE any.


    CLEAR e_t_mesg.


    IF o_use_buffer = rs_c_true.
*   yes:
      ASSIGN o_r_th_buf->* TO <l_th_buf>. "Ref a tabla de salida
      ASSIGN o_r_s_buf->*  TO <l_s_buf>."ref a work area
      <l_s_buf> = i_s_chas."i_s_chas tiene el registros que se está procesando

      "Valida que todavía no esté en la tabla
      READ TABLE <l_th_buf> INTO <l_s_buf> FROM <l_s_buf>.
      IF sy-subrc = 0.
        e_is_valid = o_r_is_valid->*. "Si ya fue validado, agrega solo el mensaje
        IF e_is_valid = rs_c_false AND e_t_mesg IS SUPPLIED.
          APPEND o_r_s_mesg->* TO e_t_mesg.
        ENDIF.
        RETURN.
      ENDIF.
    ENDIF.
* implement your algorithm to check the combination here:

    l_user = i_s_chas+44(10).
    o_utiles->check_clave(  EXPORTING
                               i_soc    = i_s_chas+4(4)
                               i_soco   = i_s_chas+30(4)
                               i_user   = l_user
                               i_cebe   = i_s_chas+20(10)
                               i_orden  = i_s_chas+8(12)
                               i_cuenta   = i_s_chas+34(10)
                               IMPORTING
                               o_result = e_is_valid
                               o_msg    = l_s_mesg
                               ).

* update the buffer with the result:
* l_s_mesg should contain a message in the 'invalid' case
    IF o_use_buffer = rs_c_true.
      o_r_is_valid->* = e_is_valid.
      IF o_r_is_valid->* = rs_c_false AND e_t_mesg IS SUPPLIED.
        o_r_s_mesg->* = l_s_mesg.
        APPEND l_s_mesg TO e_t_mesg.
      ENDIF.
      INSERT <l_s_buf> INTO TABLE <l_th_buf>.
    ENDIF.



  ENDMETHOD.


  METHOD if_rspls_cr_methods~check_hdb.
    RAISE EXCEPTION TYPE cx_rsr_x_message
      EXPORTING
        text = 'IF_RSPLS_CR_METHODS~CHECK_HDB-01-'.
  ENDMETHOD.


  METHOD if_rspls_cr_methods~create.
*---------------------------------------------------------------------*
*  --> i_tsx_seldr      selection criteria
*  <-- e_t_mesg         messages
*  <-- e_th_chas        valid combinations
*  <<- cx_rspls_failed  exception
*---------------------------------------------------------------------*

    DATA: wa_data  TYPE REF TO data,
          l_soc    TYPE /bi0/oicomp_code,
          l_soco   TYPE /bi0/oico_area,
          l_cuenta TYPE /bi0/oigl_account,
          l_cebe   TYPE /bi0/oicostcenter,
          l_orden  TYPE /bi0/oicoorder,
          l_resp   TYPE /bi0/oiusername.


    FIELD-SYMBOLS: <l_th_buf> TYPE HASHED TABLE,
                   <l_s_buf>  TYPE any,
                   <l_s_chas> TYPE any,
                   <l_s_chrt> TYPE any,
                   <l_s_soc>  TYPE any,
                   <l_s_ord>  TYPE any,
                   <l_s_cebe> TYPE any,
                   <l_s_area> TYPE any,
                   <l_s_peri> TYPE any,
                   <l_s_vari> TYPE any,
                   <l_s_cuen> TYPE any,
                   <l_s_user> TYPE any.



    CLEAR e_t_mesg.
    CLEAR e_th_chas.

*

*
    CREATE DATA wa_data LIKE LINE OF e_th_chas.
    ASSIGN wa_data->* TO <l_s_chas>.


*Asigna los campos a los punteros:
    ASSIGN COMPONENT 'CHRT_ACCTS' OF STRUCTURE <l_s_chas>
    TO <l_s_chrt>.


    ASSIGN COMPONENT 'COMP_CODE' OF STRUCTURE <l_s_chas>
     TO <l_s_soc>.


    ASSIGN COMPONENT 'COORDER' OF STRUCTURE <l_s_chas>
     TO <l_s_ord>.


    ASSIGN COMPONENT 'COSTCENTER' OF STRUCTURE <l_s_chas>
     TO <l_s_cebe>.

    ASSIGN COMPONENT 'CO_AREA' OF STRUCTURE <l_s_chas>
     TO <l_s_area>.

    ASSIGN COMPONENT 'FISCPER' OF STRUCTURE <l_s_chas>
     TO <l_s_peri>.


    ASSIGN COMPONENT 'FISCVARNT' OF STRUCTURE <l_s_chas>
     TO <l_s_vari>.

    ASSIGN COMPONENT 'GL_ACCOUNT' OF STRUCTURE <l_s_chas>
     TO <l_s_cuen>.


    ASSIGN COMPONENT 'USERNAME' OF STRUCTURE <l_s_chas>
                                     TO <l_s_user>.
*
    CALL METHOD seldr_to_range
      EXPORTING
        i_tsx_seldr = i_tsx_seldr
      IMPORTING
        e_t_range   = DATA(lt_range).

    CLEAR: l_soc, l_orden, l_cebe, l_cuenta, l_resp, l_soco.

    DATA: lr_fiscper TYPE RANGE OF /bi0/oifiscper,
          wr_fiscper LIKE LINE OF lr_fiscper,
          lr_orden   TYPE /sappo/tab_rseloption, " RANGE OF /bi0/oicoorder,
          wr_orden   LIKE LINE OF lr_orden,
          lr_cebe    TYPE /sappo/tab_rseloption, " RANGE OF /bi0/oiprofit_ctr,
          wr_cebe    LIKE LINE OF lr_cebe,
          lr_cuenta  TYPE /sappo/tab_rseloption, " RANGE OF /bi0/oigl_account,
          wr_cuenta  LIKE LINE OF lr_cuenta.


* Get selected UNIQUE values
    LOOP AT lt_range INTO DATA(ls_range).


      CASE ls_range-chanm.
        WHEN '0CO_AREA'.
          MOVE ls_range-low TO l_soco.
          MOVE ls_range-low TO  <l_s_area>.


        WHEN '0CHRT_ACCTS'.
          MOVE ls_range-low TO <l_s_chrt>.

        WHEN '0COMP_CODE'.
          MOVE ls_range-low TO l_soc.
          MOVE ls_range-low TO <l_s_soc>.

        WHEN '0COORDER'.
          MOVE-CORRESPONDING ls_range TO wr_orden.
          wr_orden-option = ls_range-compop.
          APPEND wr_orden TO lr_orden.
          MOVE ls_range-low TO <l_s_ord>.


        WHEN '0COSTCENTER'.
          MOVE-CORRESPONDING ls_range TO wr_cebe.
          wr_cebe-option = ls_range-compop.
          MOVE ls_range-low TO <l_s_cebe>.
          APPEND wr_cebe TO lr_cebe.

        WHEN '0GL_ACCOUNT'.
          MOVE ls_range-low TO <l_s_cuen>.
          MOVE-CORRESPONDING ls_range TO wr_cuenta.
          wr_cuenta-option = ls_range-compop.
          APPEND wr_cuenta TO lr_cuenta.



        WHEN '0FISCPER'.
          MOVE ls_range-low TO <l_s_peri>.
          MOVE-CORRESPONDING ls_range TO wr_fiscper.
          wr_fiscper-option = ls_range-compop.
          APPEND wr_fiscper TO lr_fiscper.



        WHEN '0FISCVARNT'.
          MOVE ls_range-low TO <l_s_vari>.


        WHEN '0USERNAME'.
          MOVE ls_range-low TO l_resp.
          MOVE ls_range-low TO <l_s_user>.


      ENDCASE.
    ENDLOOP.



    o_utiles->get_claves(
      EXPORTING
        i_soc    = l_soc
        i_soco   = l_soco
        i_cuenta = lr_cuenta
        i_cebe   = lr_cebe
        i_orden  = lr_orden
        i_user   = l_resp
    IMPORTING
      o_claves = DATA(lt_claves)
      o_result = DATA(l_result)
      o_msg    = DATA(l_msg)
    ).

    IF lt_claves[] IS NOT INITIAL.



      LOOP AT lt_claves ASSIGNING FIELD-SYMBOL(<fs_clave>).

        MOVE 'SQS' TO <l_s_chrt>.
        MOVE  l_soc TO <l_s_soc>.
        IF  <l_s_area> IS ASSIGNED.
          MOVE  l_soco TO  <l_s_area>.
        ENDIF.
        MOVE <fs_clave>-coorder TO <l_s_ord>.
        MOVE <fs_clave>-costcenter TO <l_s_cebe>.
        MOVE <fs_clave>-gl_account TO <l_s_cuen>.
        MOVE l_resp  TO <l_s_user>.
        IF <l_s_vari> IS ASSIGNED.
          MOVE 'K4' TO <l_s_vari>.
        ENDIF.

*        loop at o_utiles->gt_period_pxq ASSIGNING FIELD-SYMBOL(<fs_periodo>).
*        IF <l_s_peri> IS ASSIGNED.
*          MOVE <fs_periodo>-plan TO <l_s_peri>.
*        ENDIF.


        INSERT <l_s_chas> INTO TABLE e_th_chas.


      ENDLOOP.


    ELSE." NO se encontro el responsable

    ENDIF.

    APPEND l_msg TO e_t_mesg.

*

* implement your algorithm to create valid combinations
* make sure that only records contained in the selection
* table i_tsx_seldr will be generated here!


* begin of example code:
* use the buffer?
* o_use_buffer is switched on by default in the constructor
    IF o_use_buffer = rs_c_true.
*   update the buffer with the created records
      ASSIGN o_r_th_buf->* TO <l_th_buf>.
      ASSIGN o_r_s_buf->*  TO <l_s_buf>.
      LOOP AT e_th_chas ASSIGNING <l_s_chas>.
        <l_s_buf>       = <l_s_chas>.
        o_r_is_valid->* = rs_c_true.
        INSERT <l_s_buf> INTO TABLE <l_th_buf>.
      ENDLOOP.
    ENDIF.
* end of example code:

  ENDMETHOD.


  METHOD if_rspls_cr_methods~create_hdb.

    RAISE EXCEPTION TYPE cx_rsr_x_message
      EXPORTING
        text = 'IF_RSPLS_CR_METHODS~CREATE_HDB-01-'.
  ENDMETHOD.


  METHOD if_rspls_cr_methods~derive.
*---------------------------------------------------------------------*
*  <-- e_t_mesg         messages
*  <-> c_s_chas         characteristic combination: source and target
*                       fields included; do not change the source
*                       fields
*  <<- cx_rspls_failed  exception
*---------------------------------------------------------------------*

* begin example code:
* infrastructure needed by the buffer:
* Attention: The system also supports external buffering,
* check documentation of attribute N_USE_EXTERNAL_BUFFER
* and note 1067433
    DATA: l_s_mesg   TYPE if_rspls_cr_types=>tn_s_mesg,
          l_is_valid TYPE rs_bool.

    FIELD-SYMBOLS: <l_th_buf> TYPE HASHED TABLE,
                   <l_s_buf>  TYPE any.
* end example code:

    BREAK-POINT.
*  CLEAR e_t_mesg.
*
    IF o_use_buffer = rs_c_true.
**   yes:
*    ASSIGN o_r_th_buf_d->* TO <l_th_buf>.
*    ASSIGN o_r_s_buf->*    TO <l_s_buf>.
*    <l_s_buf> = c_s_chas.
*    READ TABLE <l_th_buf> INTO <l_s_buf> FROM <l_s_buf>.
*    IF sy-subrc = 0.
*      IF o_r_is_valid->* = rs_c_true.
*        c_s_chas = <l_s_buf>.
*        RETURN.
*      ELSE.
*        IF e_t_mesg IS SUPPLIED.
*          APPEND o_r_s_mesg->* TO e_t_mesg.
*        ENDIF.
*        RAISE EXCEPTION TYPE cx_rspls_failed
*          EXPORTING
*            msgid = o_r_s_mesg->msgid
*            msgty = o_r_s_mesg->msgty
*            msgno = o_r_s_mesg->msgno
*            msgv1 = o_r_s_mesg->msgv1
*            msgv2 = o_r_s_mesg->msgv2
*            msgv3 = o_r_s_mesg->msgv3
*            msgv4 = o_r_s_mesg->msgv4.
*      ENDIF.
*    ENDIF.
    ENDIF.
**
*
*
*
*BREAK-POINT.
*    o_utiles->derive_user(  EXPORTING
*                               i_soc    = i_s_chas+4(4)
*                               i_soco   = i_s_chas+30(4)
*                               i_cuenta = i_s_chas+43(10)
*                               i_cebe   = i_s_chas+20(10)
*                               i_orden  = i_s_chas+8(12)
*                               i_user   = i_s_chas+53(12)
*                               IMPORTING
*                               o_result = e_is_valid
*                               o_msg    = l_s_mesg
*                               ).


*l_s_mesg should contain a message in the 'invalid' case
* l_is_valid should indicate whether derivation was possible
* <l_s_buf> should contain the derived fields
*  IF o_use_buffer = rs_c_true.
*    o_r_is_valid->* = l_is_valid.
*
*
*    IF o_r_is_valid->* = rs_c_true.
*      INSERT <l_s_buf> INTO TABLE <l_th_buf>.
*      c_s_chas = <l_s_buf>.
*    ELSE.
*
*      IF e_t_mesg IS SUPPLIED.
*        o_r_s_mesg->* = l_s_mesg.
*        APPEND l_s_mesg TO e_t_mesg.
*      ENDIF.
*
*      INSERT <l_s_buf> INTO TABLE <l_th_buf>.
*
*      RAISE EXCEPTION TYPE cx_rspls_failed
*        EXPORTING
*          msgid = l_s_mesg-msgid
*          msgty = l_s_mesg-msgty
*          msgno = l_s_mesg-msgno
*          msgv1 = l_s_mesg-msgv1
*          msgv2 = l_s_mesg-msgv2
*          msgv3 = l_s_mesg-msgv3
*          msgv4 = l_s_mesg-msgv4.
*    ENDIF.
*  ENDIF.


  ENDMETHOD.


  METHOD if_rspls_cr_methods~derive_hdb.
    RAISE EXCEPTION TYPE cx_rsr_x_message
      EXPORTING
        text = 'IF_RSPLS_CR_METHODS~DERIVE_HDB-01-'.
  ENDMETHOD.


  METHOD seldr_to_range.
*---------------------------------------------------------------------*
*  --> i_tsx_seldr   selection table (contains here only cart. product
*                    selection with CHAVLs but in complex OLAP format)
*  <-- e_t_range     selection table in simple range table format
*---------------------------------------------------------------------*

    DATA: l_s_range TYPE rsdri_s_range,
          l_s_rng   TYPE rrrangesid.

    FIELD-SYMBOLS: <l_sx_seldr> TYPE rsdd_sx_seldr.

    CLEAR e_t_range.
    LOOP AT i_tsx_seldr ASSIGNING <l_sx_seldr>.
      l_s_range-chanm = <l_sx_seldr>-chanm.
      LOOP AT <l_sx_seldr>-range-range INTO l_s_rng.
        l_s_range-sign   = l_s_rng-sign.
        l_s_range-compop = l_s_rng-opt.
        l_s_range-low    = l_s_rng-low.
        l_s_range-high   = l_s_rng-high.
        APPEND l_s_range TO e_t_range.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
