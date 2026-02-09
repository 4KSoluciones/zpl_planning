*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPL_ORDER_CONV..................................*
DATA:  BEGIN OF STATUS_ZPL_ORDER_CONV                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPL_ORDER_CONV                .
CONTROLS: TCTRL_ZPL_ORDER_CONV
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPL_ORDER_CONV                .
TABLES: ZPL_ORDER_CONV                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
