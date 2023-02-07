"Name: \PR:SAPLMRMP\FO:OFFICIAL_NUMBERING_FILL_XBLNR\SE:END\EI
ENHANCEMENT 0 ZFIENH001_DOCADI.
data: e_belnr type bkpf-belnr, e_gjahr type bkpf-gjahr.
 DATA: BEGIN OF et_bseg OCCURS 0.
          INCLUDE STRUCTURE bseg.
  DATA: END OF et_bseg.
READ TABLE lt_bkpf with key blart = 'RE'.
IF sy-subrc eq 0.
 e_belnr = lt_bkpf-belnr.
 e_gjahr = lt_bkpf-gjahr.

 EXPORT e_belnr TO MEMORY  ID 'E_BELNR'.
 EXPORT E_GJAHR TO MEMORY ID 'E_GJAHR'.

 LOOP AT lt_bseg
   where belnr = lt_bkpf-belnr.
   IF lt_bseg-koart = 'K'.
     MOVE-CORRESPONDING  lt_bseg to et_bseg.
     append et_bseg.
     CLEAR et_bseg.
   ENDIF.

 ENDLOOP.

EXPORT et_bseg[] TO MEMORY ID 'ET_BSEG'.

ENDIF.

ENDENHANCEMENT.
