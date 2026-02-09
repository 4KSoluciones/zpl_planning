
*&---------------------------------------------------------------------*
*& Report  ZSAP_INFOCUBE_DESIGNS                                       *
*&                                                                     *
*&---------------------------------------------------------------------*
report  zsap_infocube_designs                   .
tables : rsdgparam.

type-pools: rsd, rsdu,rsatr,rssm,rsenq,rsods,rs,sscr.

data: g_t_tlogo        type rs_t_tlogo,
      s_tlogo          type rs_tlogo.

data: l_t_cube      type rsd_t_cube,
      l_s_cube      type rsd_s_cube,
      l_factrows    type i,
      l_s_tablsize  type rsdu_s_tablsize,
      l_t_tablsize  type rsdu_t_tablsize,
      l_density     type f,
      l_pdensity    type p decimals 1,
      l_counter     type i,
      l_odd         type i.
data: l_t_infocube      type rsd_t_infocube.

data :  wa_opt_list      type sscr_opt_list,
        wa_ass           type sscr_ass,
        wa_restrict      type sscr_restrict.

select-options
  i_icube     for rsdgparam-infocube
                 no intervals obligatory.

initialization.
  move 'JUST_EQ'     to wa_opt_list-name.
  move 'X'           to wa_opt_list-options-eq.
  append wa_opt_list to wa_restrict-opt_list_tab.

  move: 'S'        to wa_ass-kind,
        'I_ICUBE'  to wa_ass-name,
        'I'        to wa_ass-sg_main,
        ' '        to wa_ass-sg_addy,
        'JUST_EQ'  to wa_ass-op_main.
  append wa_ass    to wa_restrict-ass_tab.

  call function 'SELECT_OPTIONS_RESTRICT'
    exporting
      restriction = wa_restrict.

at selection-screen on value-request for i_icube-low.
  clear g_t_tlogo.
  append rs_c_tlogo-infocube      to g_t_tlogo.
  call method cl_rsd_dta=>f4
    exporting
      i_tlogo    = s_tlogo
      i_t_tlogo  = g_t_tlogo
    changing
      c_infoprov = i_icube-low.

start-of-selection.
  loop at i_icube.
    append i_icube-low to l_t_infocube.
  endloop.

* get list of all active infocubes in the system
  call function 'RSD_CUBE_MULTI_GET_ONLY_DB'
       exporting
            i_read_all      = rs_c_false
            i_t_infocube    = l_t_infocube
            i_objvers       = rs_c_objvers-active
*         I_CUBETYPE      = 'B'
*         I_T_CUBETYPE    =
*         I_WITH_ATR_NAV  = RS_C_FALSE
       importing
            e_t_cube        = l_t_cube.


* loop over the cubes and get information
  l_counter = 0.

  loop at l_t_cube into l_s_cube.

* get cube layout information
    call function 'RSDU_INFOCUBE_TABLE_SIZES'
      exporting
        i_infocube   = l_s_cube-infocube
        i_check      = rs_c_true
      importing
        e_factrows   = l_factrows
        e_t_tablsize = l_t_tablsize
        e_density    = l_density.

* give details only for non-empty cubes
*    if l_factrows = 0. continue. endif.

* count non-empty cubes
    l_counter = l_counter + 1.

* set format
    format color col_heading  inverse on.

* print cube infos
    l_pdensity = l_density.
    write: at /   l_s_cube-infocube,
           at 20  'rows:', l_factrows,
           at 40  'density:', l_pdensity, '%'.

* print cube table infos
    loop at l_t_tablsize into l_s_tablsize.
*   swap formats
      l_odd = l_counter mod 2.
      if l_odd = 1.
        format color col_background intensified off inverse off.
      else.
        format color col_background intensified on inverse off.
      endif.
*   write ...
      if l_s_tablsize-size_check <> rsdu_c_check-green.
        format color col_negative intensified off inverse on.
      endif.
      write: at /   l_s_cube-infocube,
             at 20  l_s_tablsize-tablnm,
             at 40  'rows:', l_s_tablsize-rows,
             at 60  'ratio:', l_s_tablsize-percent, '%'.

    endloop.
    skip.
  endloop.
