class ZPL_A07_DS_EXIT definition
  public
  create protected .

*"* public components of class CL_RSPLS_DS_EXIT_BASE
*"* do not include other source files here!!!
public section.
  type-pools RSD .

  interfaces IF_RSPLS_DATASLICE .
  interfaces IF_RSPLS_DS_EXIT .
  interfaces IF_RSPLS_DS_METHODS .
  interfaces IF_RSPLS_DS_TYPES .

  data N_BASIC_PROV type RSINFOPROV read-only .
  data N_DSNR type RSPLS_DSNR read-only .

  interface IF_RSPLS_DS_TYPES load .
  methods CONSTRUCTOR
    importing
      !I_BASIC_PROV type RSINFOPROV
      !I_DSNR type IF_RSPLS_DS_TYPES=>TN_DSNR
      !I_TS_IOBJ type IF_RSPLS_DS_TYPES=>TN_TS_IOBJ
    raising
      CX_RSPLS_FAILED .
protected section.
*"* protected components of class CL_RSPLS_DS_EXIT_BASE
*"* do not include other source files here!!!

  types TO_R_PROTECTED type ref to CHAR1 .
  types TO_R_MESG type ref to IF_RSPLS_CR_TYPES=>TN_S_MESG .

  data O_R_STRUCDESCR type ref to CL_ABAP_STRUCTDESCR .
  data O_R_S_BUF type ref to DATA .
  data O_R_TH_BUF type ref to DATA .
  data O_R_PROTECTED type TO_R_PROTECTED .
  data O_R_S_MESG type TO_R_MESG .
  data O_USE_BUFFER type RS_BOOL .

  interface IF_RSPLS_DS_TYPES load .
  class-methods GET_INSTANCE_O
    importing
      !I_BASIC_PROV type RSINFOPROV
      !I_DSNR type IF_RSPLS_DS_TYPES=>TN_DSNR
      !I_TS_IOBJ type IF_RSPLS_DS_TYPES=>TN_TS_IOBJ
      !I_CLSNAME type SEOCLSNAME optional
    returning
      value(R_R_INSTANCE) type ref to IF_RSPLS_DATASLICE
    raising
      CX_RSPLS_FAILED .
  methods SELDR_TO_RANGE
    importing
      !I_TSX_SELDR type RSDD_TSX_SELDR
    exporting
      !E_T_RANGE type RSDRI_T_RANGE .
private section.
*"* private components of class CL_RSPLS_DS_EXIT_BASE
*"* do not include other source files here!!!

  types:
    BEGIN OF tp_sx_instance,
           basic_prov    TYPE rsinfoprov,
           ts_iobj       TYPE IF_RSPLS_DS_TYPES=>TN_TS_IOBJ,
           r_instance    TYPE REF TO IF_RSPLS_DATASLICE,
         END OF tp_sx_instance .
  types:
    tp_tsx_instance TYPE SORTED TABLE OF tp_sx_instance
                         WITH NON-UNIQUE KEY basic_prov
                         INITIAL SIZE 0 .

  class-data P_TSX_INSTANCE type TP_TSX_INSTANCE .
ENDCLASS.



CLASS ZPL_A07_DS_EXIT IMPLEMENTATION.


METHOD constructor.
*---------------------------------------------------------------------*
*  --> i_basic_prov     basis infoprovider
*  --> i_dsnr           dataslice number
*  --> i_ts_iobj        characteristics
*---------------------------------------------------------------------*

  DATA:
    l_s_iobj     TYPE if_rspls_ds_types=>tn_s_iobj,
    l_s_field    TYPE if_rspls_ds_types=>tn_s_field,
    l_r_cha_prop TYPE REF TO if_rsd_cha_prop,
    l_t_comp     TYPE cl_abap_structdescr=>component_table,
    l_s_comp     TYPE cl_abap_structdescr=>component,
    l_ts_chanm   TYPE rsd_ts_chanm,
    l_r_struc    TYPE REF TO cl_abap_structdescr,
    l_idx        TYPE i.

  FIELD-SYMBOLS:
    <l_s_buf>  TYPE any,
    <l_char>   TYPE char1,
    <l_s_mesg> TYPE if_rspls_cr_types=>tn_s_mesg.

  if_rspls_dataslice~n_basic_prov = i_basic_prov.

  n_basic_prov = i_basic_prov.
  n_dsnr       = i_dsnr.

* fill internal view of characteristics for exit dataslice
* and create a data structure containing only these characteristics
  LOOP AT i_ts_iobj INTO l_s_iobj-iobjnm.
    l_s_field-iobjnm  = l_s_iobj-iobjnm.
*   we want characteristic values in internal format
*   --> leave field 'is_sid' of l_s_field empty
    l_r_cha_prop = cl_rsd_iobj_prop_cache=>get_cha( l_s_field-iobjnm ).
*   l_r_cha_prop contains the meta data for the actual infoobject:
*   ATTENTION: method GET_FIELD_NAME contains the fieldname of BW
*   generated data structures, e.g. for reference characteristics
*   the names used in master data tables may be different !
    l_s_field-fieldnm = l_r_cha_prop->get_field_name( ).
    INSERT l_s_field INTO TABLE if_rspls_dataslice~n_ts_fields.
*   create the data structure at run time using the data element:
    l_s_comp-name = l_s_field-fieldnm.
    l_s_comp-type = l_r_cha_prop->get_datadescr( ).
    APPEND l_s_comp TO l_t_comp.
    INSERT l_s_field-fieldnm INTO TABLE l_ts_chanm.
  ENDLOOP.

* now a type description of the data records can be created:
  o_r_strucdescr = cl_abap_structdescr=>get( p_components = l_t_comp
                                             p_strict     = abap_false ).
* create infrastructure to buffer the results of method 'is_protected'
  IF lines( l_ts_chanm ) > 0.
    l_idx    = lines( l_t_comp ).
    l_s_comp-name = '_PROTECTED'.
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
    ASSIGN COMPONENT '_PROTECTED' OF STRUCTURE <l_s_buf> TO <l_char>.
*   the typed data reference points to the field '_PROTECTED'
    GET REFERENCE OF <l_char> INTO o_r_protected.
    ASSIGN COMPONENT l_idx OF STRUCTURE <l_s_buf> TO <l_s_mesg>.
*   the typed data reference points to the structure '_S_MESG'
    GET REFERENCE OF <l_s_mesg> INTO o_r_s_mesg.
    CREATE DATA o_r_th_buf LIKE HASHED TABLE OF <l_s_buf>
                           WITH UNIQUE KEY (l_ts_chanm).
    o_use_buffer = rs_c_true.
  ENDIF.

ENDMETHOD.


METHOD get_instance_o.
*---------------------------------------------------------------------*
*  --> i_basic_prov     basis infoprovider
*  --> i_step           dataslice number
*  --> i_ts_iobj        characteristics
*  --> i_clsname        class name, used for subclasses
*  <-- r_r_instance     existing or new instance
*---------------------------------------------------------------------*

  DATA: l_exists      TYPE abap_bool,
        l_sx_instance TYPE tp_sx_instance,
        l_clsname     TYPE seoclsname,
        l_r_ex        TYPE REF TO cx_rspls_failed.

  FIELD-SYMBOLS: <l_sx_instance> TYPE tp_sx_instance.

* the instance table is sorted by basic_prov
* scan the relevant records by ts_iobj
  l_exists = abap_false.
  LOOP AT p_tsx_instance ASSIGNING <l_sx_instance>
                         WHERE basic_prov = i_basic_prov.
    IF <l_sx_instance>-ts_iobj = i_ts_iobj.
      l_exists = abap_true.
      r_r_instance = <l_sx_instance>-r_instance.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF l_exists = abap_false.
    l_sx_instance-basic_prov = i_basic_prov.
    l_sx_instance-ts_iobj    = i_ts_iobj.

    TRY.

        IF i_clsname IS INITIAL.
          l_clsname = 'CL_RSPLS_DS_EXIT_BASE'.
        ELSE.
          l_clsname = i_clsname.
        ENDIF.

        CREATE OBJECT l_sx_instance-r_instance TYPE (l_clsname)
          EXPORTING
            i_basic_prov = l_sx_instance-basic_prov
            i_dsnr       = i_dsnr
            i_ts_iobj    = l_sx_instance-ts_iobj.

      CATCH cx_rspls_failed INTO l_r_ex.
        RAISE EXCEPTION l_r_ex.
    ENDTRY.

    INSERT l_sx_instance INTO TABLE p_tsx_instance.
    r_r_instance = l_sx_instance-r_instance.

  ENDIF.

ENDMETHOD.


METHOD if_rspls_dataslice~create_data_ref.
*---------------------------------------------------------------------*
*  <-- r_r_data  structure with infoobjects and fieldnames
*                as defined in table n_ts_fields
*---------------------------------------------------------------------*

  CREATE DATA r_r_data TYPE HANDLE o_r_strucdescr.

ENDMETHOD.


METHOD if_rspls_ds_exit~get_instance.
*---------------------------------------------------------------------*
*  --> i_basic_prov     basis infoprovider
*  --> i_dsnr           dataslice number
*  --> i_ts_iobj        characteristics
*  --> i_clsname        class name, used for subclasses
*  <-- r_r_instance     existing or new instance
*---------------------------------------------------------------------*

  DATA: l_r_ex TYPE REF TO cx_rspls_failed.

  TRY.

      CALL METHOD get_instance_o
        EXPORTING
          i_basic_prov = i_basic_prov
          i_dsnr       = i_dsnr
          i_ts_iobj    = i_ts_iobj
          i_clsname    = i_clsname
        RECEIVING
          r_r_instance = r_r_instance.

    CATCH cx_rspls_failed INTO l_r_ex.
      RAISE EXCEPTION l_r_ex.
  ENDTRY.

ENDMETHOD.


METHOD if_rspls_ds_methods~is_overlapping.
*---------------------------------------------------------------------*
*  --> i_tsx_seldr      selection table
*  <-- r_is_overlapping flag, selection has an overlap with with
*                       the 'dataslice' to be checked here
*---------------------------------------------------------------------*

* begin of default implementation
  r_is_overlapping = rs_c_true.
* of of default implementation

ENDMETHOD.


METHOD if_rspls_ds_methods~is_protected.
*---------------------------------------------------------------------*
*  --> i_s_data   data record, the values for infoobjects from
*                 n_ts_fields are set, the rest is initial
*  <-- e_t_mesg   messages
*  <-- e_noinput  flag, records is protected or not
*---------------------------------------------------------------------*
DATA: lo_utiles type ref to zpl_utiles.


    CLEAR e_t_mesg.

    FIELD-SYMBOLS:
      <l_th_buf> TYPE HASHED TABLE,
      <l_s_buf>  TYPE any.

      lo_utiles = new zpl_utiles( ).

    CLEAR e_t_mesg.


    ASSIGN o_r_th_buf->* TO <l_th_buf>.
    ASSIGN o_r_s_buf->* TO <l_s_buf>.
    <l_s_buf> = i_s_data.
    READ TABLE <l_th_buf> INTO <l_s_buf> FROM <l_s_buf>.
    IF sy-subrc NE 0.
* This record is not checked before
* Now we check if the record is locked


CALL METHOD lo_utiles->get_locked_entries
EXPORTING
i_s_data = i_s_data
i_link   = lo_utiles->c_varq
IMPORTING
e_s_mesg = o_r_s_mesg->*
e_noinput = o_r_protected->*.

      INSERT <l_s_buf> INTO TABLE <l_th_buf>.
    ENDIF.
    e_noinput = o_r_protected->*."fix pointer to <l_s_buf>-protected
    IF e_noinput = rs_c_true AND e_t_mesg IS SUPPLIED.
* o_r_s_mesg is a pointer to '_S_MESG' in the buffer workarea
      APPEND o_r_s_mesg->* TO e_t_mesg.
    ENDIF.

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
